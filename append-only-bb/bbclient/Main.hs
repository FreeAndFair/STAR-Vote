{-# LANGUAGE OverloadedStrings #-}

{-|

This is a simple client for the append-only BB. It allows the creation
of new identities (keypairs), the downloading of public keys from the
BB, posting to the BB with a local private key, and reading and
verifying the BB history.

The keyserver feature should probably be separated for a
non-demonstrator implementation.

-}

module Main where

import BB.DB (Message(..), Author(..), genKeypair, epsilon)
import BB.JSON
import qualified BB.Protocol as Protocol

import Control.Applicative

import Crypto.PubKey.ECC.Generate
import Crypto.Random
import Crypto.Types.PubKey.ECC
import Crypto.Types.PubKey.ECDSA

import Data.Aeson
import Data.Aeson.TH

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Data.Foldable (foldlM, foldrM)

import qualified Data.Map as M
import Data.Map (Map)

import Data.Maybe (isJust)

import Data.Text.Encoding (encodeUtf8)

import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)

import qualified Data.Text as T

import Network.HTTP

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (stdin, stdout, hGetContents, hFlush)
import GHC.IO.Handle (hDuplicate)

import Paths_append_only_bb


type Secrets = Map T.Text (PublicKey, PrivateKey)

type PublicInfo = Map T.Text PublicKey

type Boards = Map String PublicKey

-- | The name of the local file storing private kys
keyfileName :: IO String
keyfileName = getDataFileName "SECRET"

-- | The file storing public keys
publicStuffName :: IO String
publicStuffName = getDataFileName "PUBLIC"

-- | The file storing bulletin board keys
boardKeyFile :: IO String
boardKeyFile = getDataFileName "BOARDS"

-- | Read the contents of the local key files
loadInfo :: IO (Secrets, PublicInfo, Boards)
loadInfo =
  do keyfile <- keyfileName
     exists <- doesFileExist keyfile
     secrets <-
       if exists
         then do f <- BSL.readFile keyfile
                 return $ case decode f of
                            Nothing -> error "couldn't read keyfile"
                            Just contents ->
                              case (fromJSON contents :: Result Secrets) of
                                Success m -> m
                                nope -> error (show nope)
         else do let m = M.empty :: Secrets
                     c = toJSON m
                 putStrLn "Initializing secret storage..."
                 BSL.writeFile keyfile (encode c)
                 return m

     publicfile <- publicStuffName
     publicExists <- doesFileExist publicfile
     publicInfo <-
       if publicExists
         then do f <- BSL.readFile publicfile
                 return $ case decode f of
                            Nothing   -> error "couldn't read corrupt public key db"
                            Just info -> info
         else return M.empty

     boardfile <- boardKeyFile
     boardExists <- doesFileExist boardfile
     boards <-
       if boardExists
         then do f <- BSL.readFile boardfile
                 return $ case decode f of
                            Nothing   -> error "couldn't read corrupt board key db"
                            Just info -> info
         else return M.empty

     return (secrets, publicInfo, boards)

main :: IO ()
main = do (secrets, publicInfo, boards) <- loadInfo
          args <- getArgs
          case args of
            [server, "register"] -> register publicInfo secrets server 
            [server, "getkeys"]  -> getKeys publicInfo server
            [server, "read"]     -> getPosts publicInfo server boards
            [server, "post"]     -> makePost secrets publicInfo boards server
            _ -> putStrLn $ unlines [ "Usage:"
                                    , "bbclient SERVER COMMAND"
                                    , "where COMMAND is:"
                                    , "  getkeys  -  get the public keys from the server"
                                    , "  read     -  verify and read the BB"
                                    , "  post     -  post to the BB"
                                    ]

-- | Either read JSON from the server, or crash with an error report
getJSONOrDie :: (FromJSON a) => String -> IO a
getJSONOrDie url =
  do resp <- simpleHTTP $ getRequest url
     case resp of
       Left err -> error $ "Error getting URL " ++ url ++ ": " ++ show err
       Right info ->
         case decode (UTF8.fromString (rspBody info)) of
           Nothing -> error $ "Failed to decode from " ++ url ++ ": " ++
                              show (rspBody info)
           Just a -> return a

-- | Save the bulletin board's public key. If one is already on file,
-- check that they match and complain if not.
getBoardKey :: String -> Boards -> IO PublicKey
getBoardKey server boards =
  do k <- getJSONOrDie $ "http://" ++ server ++ "/pubkey.json"
     case M.lookup server boards of
       Nothing     -> do saveKeys (M.insert server k boards)
                         return k
       Just oldKey -> if k == oldKey
                        then return k
                        else error $ "Mismatching key for " ++ server
  where saveKeys :: Boards -> IO ()
        saveKeys boards =
          do boardfile <- boardKeyFile
             BSL.writeFile boardfile (encode boards)

register :: PublicInfo -> Secrets -> String -> IO ()
register public secrets server =
  do putStr "Username> "
     hFlush stdout
     name <- fmap T.pack getLine
     let found = isJust (M.lookup name secrets) || isJust (M.lookup name public)
     if found
       then error $ T.unpack name ++ " is already a known user"
       else do g <- (fmap cprgCreate createEntropyPool) :: IO SystemRNG
               let ((pub, priv), _) = genKeypair g
                   public'  = M.insert name pub         public
                   secrets' = M.insert name (pub, priv) secrets
               savePublic public'
               saveKeyfile secrets'
               -- send to server
               let request = postRequestWithBody ("http://" ++ server ++ "/register.json")
                                                 "application/json"
                                                 (UTF8.toString $ encode (name, pub))
               res <- simpleHTTP request
               case res of
                 Left err -> error $ "Couldn't send new key to server: " ++ show err
                 Right response -> putStrLn "Generated"

getKeys :: PublicInfo -> String -> IO ()
getKeys public server =
  do authors <- getJSONOrDie $ "http://" ++ server ++ "/users.json" ::
                IO [Author]
     case foldrM addUser public authors of
       Left msg -> error msg
       Right newpub ->
         do savePublic newpub
            putStrLn "Done"

  where addUser (Author name key) public =
          case M.lookup name public of
            Just k | k == key -> Right public
                   | otherwise -> Left $ "Mismatching keys for " ++ show name
            Nothing -> Right $ M.insert name key public

getPosts :: PublicInfo -> String -> Boards -> IO ()
getPosts public server boards =
  do board <- getJSONOrDie $ "http://" ++ server ++ "/bb.json" ::
              IO (Protocol.History Message Author,
                  Protocol.Signed (Protocol.Hash, UTCTime))
     now <- getCurrentTime
     boardpub <- getBoardKey server boards
     case Protocol.checkRead (\(Author n _) -> M.lookup n public)
                             5 -- server must respond in 5 seconds
                             30 -- use 30 seconds for ε
                             now
                             boardpub
                             Protocol.initialHash
                             board of
       Left err -> error err
       Right () -> mapM_ printPost . reverse . fst $ board
  where printPost (Protocol.Post m t w _) =
          do putStr "Author: "
             print (authorName w)
             putStr "Posted: "
             print t
             putStrLn "Message:"
             print (messageText m)
             putStrLn "------------------"

saveKeyfile :: Secrets -> IO ()
saveKeyfile ss = do keyfile <- keyfileName
                    BSL.writeFile keyfile (encode ss)

savePublic :: PublicInfo -> IO ()
savePublic pub = do pubfile <- publicStuffName
                    BSL.writeFile pubfile (encode pub)

compose :: Secrets -> IO (Message, Author, PublicKey, PrivateKey)
compose secrets = do let users = zip [1..] $ M.assocs secrets
                     putStrLn "Who are you?"
                     mapM_ (\(i, u) ->
                             putStrLn $ " " ++ show i ++
                             "\t" ++ T.unpack (fst u))
                           users
                     putStr "> "
                     hFlush stdout

                     choice <- getLine
                     case lookup (read choice) users of
                       Nothing -> error "Invalid choice"
                       Just (name, (pub, priv)) ->
                         do putStrLn "Message body (^D to terminate): "
                            stdin' <- hDuplicate stdin
                            body <- fmap T.pack $ hGetContents stdin'
                            return (Message body, Author name pub, pub, priv)

makePost :: Secrets -> PublicInfo -> Boards -> String -> IO ()
makePost secrets public boards server =
  do g <- (fmap cprgCreate createEntropyPool) :: IO SystemRNG
     (text, w, pub, priv) <- compose secrets
     -- Message 1: B -> W : S_B(H_\lambda, T_B)
     k <- getBoardKey server boards
     current <- getJSONOrDie $ "http://" ++ server ++ "/current-hash.json" ::
                IO Protocol.CurrentHash
     now <- getCurrentTime
     case Protocol.checkCurrentHash k now epsilon current of
       Left err -> error $ "The board at " ++ server ++ " is cheating!\n" ++ err
       Right () ->
         -- Message 2
         do let Protocol.CurrentHash (Protocol.Signed (h, _) _) = current
                (msg2, _) = Protocol.prepareMessage g priv text now w h
                request = postRequestWithBody ("http://" ++ server ++ "/post.json")
                                              "application/json"
                                              (UTF8.toString $ encode msg2)
            reply <- simpleHTTP request
            case reply of
              Left err -> error $ show err
              Right msg ->
                case eitherDecode (UTF8.fromString (rspBody msg)) ::
                     Either String (Protocol.Signed (Protocol.Signed Protocol.Hash, UTCTime)) of
                  Left err -> error err
                  Right accepted ->
                    case Protocol.checkAcceptedMessage k now epsilon accepted of
                      Left err -> error $ "Potential cheating detected! " ++ err
                      Right () -> putStrLn "Posted successfully"

