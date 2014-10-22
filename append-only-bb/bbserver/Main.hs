{-# LANGUAGE OverloadedStrings #-}
module Main where

import BB
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

import Data.Monoid

import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (parseTime)

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

data BBState = BBState { bb_randomState :: SystemRNG
                       , bb_connection :: Connection}

type BB = StateT BBState Snap

withRandom :: (SystemRNG -> BB (b, SystemRNG)) -> BB b
withRandom f = do BBState { bb_randomState = g } <- get
                  (x, g') <- f g
                  modify $ \st -> st { bb_randomState = g' }
                  return x

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

root :: Html
root = page "The Public, Append-Only Bulletin Board" $ do
         h2 "Actions"
         ul $ do
           li $ a ! href "/register" $ "Register"
           li $ a ! href "/post" $ "Post"
         h2 "State"
         ul $ do
           li $ a ! href "/users" $ "List users"

render :: Html -> BB ()
render = writeLBS . renderHtml

registerForm :: Html
registerForm =
    page "Register user" $ do
      H.form ! A.method "POST" $ do
        H.label "Name"
        input ! name "name" ! A.id "registerName"
        H.hr
        H.label "Signer info:" ; H.br
        textarea ! A.id "regInput" $ mempty
        H.hr
        H.label "Public key:" ; H.br
        textarea ! name "key" $ ""
        input ! type_ "submit"

register :: BB ()
register =
  do name <- maybe (error "no name") id <$> getPostParam "name"
     pubkey <- maybe (error "no key") (BL.fromChunks . pure) <$> getPostParam "key"
     case Aeson.decode pubkey :: Maybe PublicKey of
       Nothing -> render . page "Invalid key" $ do
                    H.p "The public key couldn't be deserialized"
       Just k -> do withDB $ flip addAuthor (Author (decodeUtf8 name) k)
                    render . page "Registered!" $ do
                      H.a ! href "/" $ "ok"

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

resetForm :: Html
resetForm =
  page "Reset bb?" $ do
    H.form ! A.method "POST" $ do
      H.p "Delete everything?"
      input ! type_ "submit" ! value "Yep!"

reset :: BB ()
reset =
  do withRandom $ \g -> withDB (initialize g)
     render . docTypeHtml $ do
       H.head $ H.title "Deleted!"
       body $ do
         h1 "Deleted!"
         H.a ! href "/" $ "ok"


-- | Get the last hash with a signed timestamp - step 1 in the posting process
post1 :: BB ()
post1 =
  do CurrentHash (Signed (hash, t) sig) <-
       withRandom $ \g -> withDB $ getCurrentHash g
     as <- authors
     now <- fmap timeString $ liftIO getCurrentTime
     render . page "Step 1: signed timestamp and hash" $ do
       H.dl (mkDl [ ("hash", H.span ! A.id "the-hash" ! dataAttribute "hash" (toValue (show hash)) $ toHtml (show hash))
                  , ("timestamp", toHtml (timeString t))
                  , ("signature", H.pre . toHtml . UTF8.toString . Aeson.encode $ sig)])
       h2 "Post your message below:"
       H.form $ do
         H.label "Message"
         textarea ! name "message" ! A.id "message" ! class_ "step1" $ mempty
         H.br
         H.label "Timestamp"
         input ! type_ "text" ! name "timestamp" ! A.id "timestamp" ! class_ "step1()" ! A.value (toValue now)
         H.br
         H.label "Author"
         H.select ! A.name "author" ! A.id "author" ! class_ "step1" $ as
         H.hr
         H.label "Signer info:" ; H.br
         textarea ! A.id "sigInput" $ mempty
         H.br
         H.button ! type_ "button" ! onclick "step1recomp" ! A.value "Recompute" $ "Recompute"
         H.hr
         H.label "Hash and signature"
         textarea ! name "hashsig" $ mempty
         H.br
         H.input ! type_ "submit" ! A.value "Submit"

  where mkDl ((t, d):rest) = H.dt t <> H.dd d <> mkDl rest
        mkDl []            = mempty

        authors = do as <- withDB $ getAuthors
                     return . mconcat . map authorOption $ as
        authorOption (i, Author {authorName = n}) = H.option ! value (toValue n) $ toHtml n



server :: FilePath -> BB ()
server js =
      (method GET . ifTop $ render root)
  <|> (method GET . path "bb" $ render mempty)
  <|> (path "register" $
           (method GET $ render registerForm)
       <|> (method POST $ register))
  <|> (path "reset" $
           (method GET $ render resetForm)
       <|> (method POST $ reset))
  <|> (path "users" $ userList)
  <|> (path "post" $ method GET $ post1)
  <|> (path "jquery.js" . serveFile $ js </> "jquery-2.1.1.min.js")
  <|> (path "server.js" . serveFile $ js </> "server.js")



main :: IO ()
main = do dbfile <- getDataFileName "bb.sqlite"
          jsdir <- getDataFileName "js"
          conn <- connectSqlite3 dbfile
          g <- (fmap cprgCreate createEntropyPool) :: IO SystemRNG
          let initialState = BBState g conn
          simpleHttpServe (setPort 8000 defaultConfig :: Config BB ()) $ evalStateT (server jsdir) initialState

