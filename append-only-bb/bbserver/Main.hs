{-# LANGUAGE OverloadedStrings #-}
module Main where

import BB.DB
import BB.JSON
import BB.Protocol
import BB.Time

import Control.Applicative
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import Crypto.PubKey.ECC.Generate
import Crypto.Random
import Crypto.Types.PubKey.ECC
import Crypto.Types.PubKey.ECDSA

import qualified Data.Aeson as Aeson

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 as UTF8

import qualified Data.HashMap.Strict as HashMap
import Data.Monoid

import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (parseTime)

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Database.HDBC
import Database.HDBC.Sqlite3

import Snap.Core
import Snap.Http.Server
import Snap.Http.Server.Config
import Snap.Util.FileServe

import System.FilePath.Posix ((</>))
import System.Environment (getArgs)
import System.IO (stdout, hFlush)
import System.Locale (defaultTimeLocale)


import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (id, method)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

import Paths_append_only_bb

-- | The internal state of the bulletin board monad
data BBState = BBState { bb_randomState :: SystemRNG
                       , bb_connection :: Connection }

type BB = StateT BBState Snap


-- | Use the monad's internal random state to run a function needing it
withRandom :: (SystemRNG -> BB (b, SystemRNG)) -> BB b
withRandom f = do BBState { bb_randomState = g } <- get
                  (x, g') <- f g
                  modify $ \st -> st { bb_randomState = g' }
                  return x

-- | Use the monad's internal DB connection to run a DB action The
-- internal action is IO rather than BB for convenient operation with
-- the database primitives that don't know about BB.
withDB :: (Connection -> IO a) -> BB a
withDB f = do BBState { bb_connection = c } <- get
              liftIO $ f c

-- | Basic page template
page :: String -> Html -> Html
page title contents =
  docTypeHtml $ do
    H.head $ do
      H.title (toHtml title)
      mconcat $ map scriptSrc ["/jquery.js", "/server.js"]
    body $ do
      h1 (toHtml title)
      contents
  where scriptSrc :: String -> Html
        scriptSrc s = script ! src (toValue s) $ mempty

-- | Error page template
errorPage :: String -> Html -> Html
errorPage title contents = page ("Error: " ++ title) contents

-- | The root of the interface - provides a simple interface for
-- reading.
root :: Html
root = page "The Public, Append-Only Bulletin Board" $ do
         h2 "Actions"
         ul $ do
           li $ a ! href "/reset" $ "Delete everything"
         h2 "State"
         ul $ do
           li $ a ! href "/bb" $ "List posts"
           li $ a ! href "/users" $ "List users"

-- | Render a Blaze structure to the Snap response
render :: Html -> BB ()
render = writeLBS . renderHtml

-- | Render a structure as JSON to the Snap response
sendJSON :: (Aeson.ToJSON a) => a -> BB ()
sendJSON = writeLBS . Aeson.encode

-- | List the known BB users in a table
userList :: BB ()
userList =
  do users <- withDB getAuthors
     render . page "Authors" $ do
       table . mconcat $ map showAuthor users
       H.a ! href "/" $ "ok"
  where
    showAuthor :: (Integer, Author) -> Html
    showAuthor (_, Author { authorName = n, authorPubKey = k }) =
      tr $ do td $ toHtml n
              td (pre . toHtml . decodeUtf8 . mconcat . BL.toChunks $ Aeson.encode k)

-- | List the known users as a JSON structure, for machine consumption
userListJSON :: BB ()
userListJSON = withDB getAuthors >>= sendJSON . map snd

-- | List the posts as a JSON structure, for machine consumption. This
-- is Message 1 (ie the only message) in the reading protocol.
postListJSON :: BB ()
postListJSON = do hist <- withDB getHistory
                  (_, priv) <- withDB getKeypair
                  now <- liftIO getCurrentTime
                  msg1 <- withRandom $ \g -> return $ readBoard g hist priv initialHash now
                  sendJSON msg1

-- | Send the board's public key as JSON, so clients can verify signatures
pubKeyJSON :: BB ()
pubKeyJSON = do (pub, _) <- withDB getKeypair
                sendJSON pub

-- | Send a signed copy of the current state hash and the current
-- time. This is Message 1 in the writing protocol.
postMsg1JSON :: BB ()
postMsg1JSON = do current <- withRandom $ \g -> withDB $ getCurrentHash g
                  sendJSON current


-- | Receive a new message (Message 2) from the client, check it for
-- validity, and save the new post and return Message 3 if it's valid.
postMsg2JSON :: BB ()
postMsg2JSON =
  do paramMsg <- readRequestBody 1000000
     (_, priv) <- withDB getKeypair
     case Aeson.eitherDecode' paramMsg ::
          Either String (NewMessage Message Author) of
       Left err -> sendJSON $ "Couldn't decode message: " ++ err
       Right msg@(NewMessage p tw w@(Author n k) h) ->
         do dbkey <- withDB $ \db -> getAuthorKey db n
            h0 <- withDB getStateHash
            case dbkey of
              Left err -> sendJSON $ "Couldn't fetch user " ++ T.unpack n ++ ": " ++ err
              Right k' ->
                if k /= k'
                  then do liftIO . putStrLn $ "Mismatching keys for user " ++ T.unpack n
                          error $ "Mismatching keys for user " ++ T.unpack n
                  else case checkMessage h0 k msg of
                         Left err -> do liftIO $ putStrLn err
                                        fail err
                         Right () -> do now <- liftIO getCurrentTime
                                        accepted <-
                                          withRandom $ \g -> return $
                                            acceptedMessage g priv h now
                                        withDB $ \c -> addPost c $ Post p tw w accepted
                                        sendJSON accepted

-- | Receive a new user registration and save the associated public key
registerJSON :: BB ()
registerJSON =
  do paramUser <- readRequestBody 1000000
     case Aeson.eitherDecode' paramUser :: Either String (T.Text, PublicKey) of
       Left err -> sendJSON $ "Couldn't read registration: " ++ err
       Right (name, pub) ->
         do withDB $ flip addAuthor (Author name pub)
            sendJSON ("ok" :: String)

-- | Ask whether or not to delete everything and reset
resetForm :: Html
resetForm =
  page "Reset bb?" $
    H.form ! A.method "POST" $ do
      H.p "Delete everything?"
      input ! type_ "submit" ! value "Yep!"

-- | Show the contents of the BB
postList :: BB ()
postList = do hist <- withDB getHistory
              render . page "Post history" $ do
                H.table $ do
                  theader
                  mconcat . map renderPost . reverse $ hist
                H.a ! A.href "/" $ "ok"
  where renderPost post = H.tr $ do
                            H.td . toHtml . authorName . postWriter $ post
                            H.td . toHtml . timeString . postWrittenTime $ post
                            H.td . H.pre . toHtml . messageText . postMessage $ post
        theader = H.tr $ mconcat [H.th "Author", H.th "Timestamp", H.th "Text"]

-- | Actually reset everything
reset :: BB ()
reset =
  do withRandom $ \g -> withDB (initialize g)
     render . docTypeHtml $ do
       H.head $ H.title "Deleted!"
       body $ do
         h1 "Deleted!"
         H.a ! href "/" $ "ok"

-- |  The actual server
server :: FilePath -> BB ()
server js =
      (method GET . ifTop $ render root)
  <|> (method GET . path "pubkey.json" $ pubKeyJSON)
  <|> (method GET . path "bb" $ postList)
  <|> (method GET . path "bb.json" $ postListJSON)
  <|> (method GET . path "current-hash.json" $ postMsg1JSON)
  <|> (method POST . path "post.json" $ postMsg2JSON)
  <|> (method POST . path "register.json" $ registerJSON)
  <|> (path "reset" $
           (method GET $ render resetForm)
       <|> (method POST reset))
  <|> (path "users" userList)
  <|> (path "users.json" userListJSON)
  <|> (path "jquery.js" . serveFile $ js </> "jquery-2.1.1.min.js")
  <|> (path "server.js" . serveFile $ js </> "server.js")



main :: IO ()
main = do dbfile <- getDataFileName "bb.sqlite"
          jsdir <- getDataFileName "js"
          conn <- connectSqlite3 dbfile
          g <- fmap cprgCreate createEntropyPool :: IO SystemRNG
          let initialState = BBState g conn
          serverConfig <- commandLineConfig mempty :: IO (Config BB ())
          simpleHttpServe serverConfig $
            evalStateT (server jsdir) initialState

