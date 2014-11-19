{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Data.Acid (openLocalStateFrom)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base16.Lazy as B16
import           Data.Default (def)
import           Data.Int (Int64)
import           Data.Text.Lazy (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Snap.Core
import           Snap.Util.FileServe
import           System.Environment (getEnv)

import           Application.Star.HashChain
import           Application.Star.SerializableBS (SerializableBS(SB), fromSB)
import           Application.Star.Util (statefulErrorServe)
import           Application.StarTerminal.Controller
import           Application.StarTerminal.State (Terminal(..), TerminalState(..))

import           Paths_star_terminal (getDataFileName)

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

  pubkey <- decode'                             <$> getEnv "STAR_PUBLIC_KEY"
  zp     <- PublicHash   . decode 32            <$> getEnv "STAR_INIT_PUBLIC_HASH"
  zi     <- InternalHash . decode 32            <$> getEnv "STAR_INIT_INTERNAL_HASH"
  z0     <- fromSB       . decode 32            <$> getEnv "STAR_PUBLIC_SALT"
  url    <-                                         getEnv "STAR_POST_VOTE_URL"
  let term = Terminal tId pubkey zp zi z0 url
      defaultState = TerminalState def def term

  stateFile <- getDataFileName ("terminalState" ++ tIdStr)
  putStrLn $ "The state file is " ++ stateFile ++ ". Delete it to reconfigure the terminal."
  state <- openLocalStateFrom stateFile defaultState 

  statefulErrorServe site state

-- | Defines URLs and request methods associated with each server handler.
site :: StarTerm m => m ()
site =
    -- ifTop (formHandler) <|>
    route [ ("ballots",                    method GET  askForBallotCode)
          , ("ballots/:code/step/:stepId", method GET  showBallotStep)
          , ("ballots/:code/step/:stepId", method POST recordBallotSelection)
          , ("ballots/:code/summary",      method GET  showSummary)
          , ("ballots/:code/summary",      method POST finalize)
          , ("ballots/:code/complete",     method GET  exitInstructions)
          , ("ballots/:ballotId/codes/:code",  method POST recordBallotStyleCode)
          ] <|>
    dir "static" (serveDirectory "static")

decode :: Int64 -> String -> SerializableBS
decode n s = if BS.length bs == n then SB bs else
  error $ "Length of input should be " ++ show n ++ " bytes."
  where
    bs = decode' s

decode' :: String -> ByteString
decode' = fst . B16.decode . encodeUtf8 . pack
