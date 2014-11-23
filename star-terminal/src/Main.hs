{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Runs the voting terminal's internal web server
 -}
module Main
  ( main
  , site
  ) where

import           Control.Applicative
import           Control.Monad                       (void)
import           Data.Acid                           (openLocalStateFrom, query)
import qualified Data.ByteString.Base16.Lazy         as B16
import           Data.ByteString.Lazy                (ByteString)
import qualified Data.ByteString.Lazy                as BS
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Default                        (def)
import           Data.Int                            (Int64)
import           Data.Monoid
import           Data.Text.Lazy                      (pack, unpack)
import           Data.Text.Lazy.Encoding             (encodeUtf8, decodeUtf8)
import           Network.HTTP.Client                 (Request,
                                                      RequestBody (..),
                                                      httpNoBody, parseUrl,
                                                      urlEncodedBody,
                                                      withManager)
import qualified Network.HTTP.Client                 as HTTP
import           Network.HTTP.Client.TLS             (tlsManagerSettings)
import           Snap.Core
import           Snap.Http.Server.Config             (Config, commandLineConfig,
                                                      getHostname, getPort)
import           Snap.Util.FileServe
import           System.Environment                  (getEnv)

import           Application.Star.HashChain
import           Application.Star.SerializableBS     (SerializableBS (SB),
                                                      fromSB)
import           Application.Star.Util               (statefulErrorServe)
import           Application.StarTerminal.Controller
import           Application.StarTerminal.State      (GetRegisterURL (..),
                                                      Terminal (..),
                                                      TerminalState (..))

import           Paths_star_terminal                 (getDataFileName)

-- | Main function for the @star-terminal@ executable.
-- Collects configuration paramaters from environment variables.
-- See @Application.StarTerminal.State@ for documentation on parameters.
--
-- @star-terminal@ may be run with a @-p@ flag to specify a port number.
--
-- The first time @star-terminal@ is run, it saves its configuration
-- and state. Subsequent invocations remember this. Delete the state
-- file to reset.
main :: IO ()
main = do
  -- Construct the default terminal configuration to use on first execution
  tIdStr <- getEnv "STAR_TERMINAL_ID"
  let tId = TerminalId . SB . encodeUtf8 . pack  $  tIdStr

  pubkey  <- decode'                  <$> getEnv "STAR_PUBLIC_KEY"
  zp      <- PublicHash   . decode 32 <$> getEnv "STAR_INIT_PUBLIC_HASH"
  zi      <- InternalHash . decode 32 <$> getEnv "STAR_INIT_INTERNAL_HASH"
  z0      <- fromSB       . decode 32 <$> getEnv "STAR_PUBLIC_SALT"
  voteURL <-                              getEnv "STAR_POST_VOTE_URL"
  regURL  <-                              getEnv "STAR_REGISTER_URL"

  let term = Terminal tId pubkey zp zi z0 voteURL regURL
      defaultState = TerminalState def def term

  stateFile <- getDataFileName ("terminalState" ++ tIdStr)
  putStrLn $ "The state file is " ++ stateFile ++ ". Delete it to reconfigure the terminal."
  state <- openLocalStateFrom stateFile defaultState

  -- Register own address with the controller.  Read from state
  -- because saved state is more important than environment vars.
  snapConfig <- commandLineConfig mempty :: IO (Config Snap ()) -- dummy type because it doesn't matter here
  regURL' <- query state GetRegisterURL
  register regURL' snapConfig

  statefulErrorServe site state

  where register :: String -> Config m a -> IO ()
        register controllerURL cfg = maybe (error "Can't get host info for registration")
                                           doPost
                                           myURL
          where doPost u = do
                  req <- parseUrl controllerURL
                  let req' = urlEncodedBody [("url", u)] $ req { HTTP.method = "POST" }
                  void $ withManager tlsManagerSettings (httpNoBody req')
                  return ()

                myURL = do host <- getHostname cfg
                           port <- getPort cfg
                           return . mconcat . BS.toChunks $ 
                             "http://" <> BS.fromChunks [host] <>
                             ":" <> Char8.pack (show port) <>
                             "/ballots/"

-- | Defines URLs and request methods associated with each server handler.
site :: StarTerm m => m ()
site =
    -- ifTop (formHandler) <|>
    route [ ("ballots",                       method GET  askForBallotCode)
          , ("ballots/:code/step/:stepId",    method GET  showBallotStep)
          , ("ballots/:code/step/:stepId",    method POST recordBallotSelection)
          , ("ballots/:code/summary",         method GET  showSummary)
          , ("ballots/:code/summary",         method POST finalize)
          , ("ballots/:code/complete",        method GET  exitInstructions)
          , ("ballots/:ballotId/codes/:code", method POST recordBallotStyleCode)
          ] <|>
    dir "static" (serveDirectory "static")

decode :: Int64 -> String -> SerializableBS
decode n s = if BS.length bs == n then SB bs else
  error $ "Length of input should be " ++ show n ++ " bytes."
  where
    bs = decode' s

decode' :: String -> ByteString
decode' = fst . B16.decode . encodeUtf8 . pack
