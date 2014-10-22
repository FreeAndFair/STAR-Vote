{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import BB.JSON
import BB.Protocol (CurrentHash)
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

import qualified Data.Map as M
import Data.Map (Map)

import Data.Text.Encoding (encodeUtf8)

import Data.Time (UTCTime, getCurrentTime)

import qualified Data.Text as T

import System.Directory (doesFileExist)
import System.Environment (getArgs)

import Paths_append_only_bb



type Secrets = Map T.Text (PublicKey, PrivateKey)

type PublicInfo = Map T.Text PublicKey


data ClientCmd = Keygen T.Text
               | VerifyTimestamp CurrentHash PublicKey

instance FromJSON ClientCmd where
  parseJSON (Object v) =
    do cmd <- v .: "command"
       case T.unpack cmd of
         "keygen" ->
           Keygen <$> v.: "name"
         "verify-timestamp-sig" ->
           VerifyTimestamp <$>
             v .: "timestamp-sig" <*>
             v .: "public-key"
         other              -> fail $ "Unknown command: " ++ other
  parseJSON _          = fail "Must be an object"


keyfileName :: IO String
keyfileName = getDataFileName "SECRET"

publicStuffName :: IO String
publicStuffName = getDataFileName "PUBLIC"

loadInfo :: IO (Secrets, Maybe PublicInfo)
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
                            Nothing   -> error "couldn't read public key db"
                            Just info -> Just info
         else return Nothing
     return (secrets, publicInfo)

main :: IO ()
main = do (secrets, publicInfo) <- loadInfo
          cmd <- BSL.getContents
          case eitherDecode cmd of
            Left err -> putStrLn $ "didn't understand: " ++ err
            Right cmd -> process secrets publicInfo cmd

saveKeyfile :: Secrets -> IO ()
saveKeyfile ss = do keyfile <- keyfileName
                    BSL.writeFile keyfile (encode ss)

process :: Secrets -> Maybe PublicInfo -> ClientCmd -> IO ()
process ss pub (Keygen name) =
  case M.lookup name ss of
    Just (pub, _) -> do putStrLn "Found public key:"
                        putStrLn  . UTF8.toString . encode $ pub
    Nothing -> do (pub, priv) <- genKeypair
                  let ss' = M.insert name (pub, priv) ss
                  putStrLn $ "Created key for " ++ show name
                  putStrLn . UTF8.toString . encode $ pub
                  saveKeyfile ss'
  where
    genKeypair = do g <- fmap cprgCreate createEntropyPool
                    let c = getCurveByName SEC_p112r1 -- TODO pick one non-arbitrarily
                    let ((pub, priv), g') = generate (g::SystemRNG) c
                    return (pub, priv)

process ss _ (VerifyTimestamp current k) =
  do now <- getCurrentTime
     let epsilon = 30
     case Protocol.checkCurrentHash k now 30 current of
       Left err -> putStrLn $ "Couldn't verify: " ++ err
       Right () -> putStrLn $ "OK, proceed"

