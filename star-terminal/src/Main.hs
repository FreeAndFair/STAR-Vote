{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import qualified Data.Binary as B
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
import           System.Random (getStdGen)

import           Application.Star.HashChain
import           Application.Star.SerializableBS (SerializableBS(SB))
import           Application.Star.Util (statefulErrorServe)
import           Application.StarTerminal.Controller
import           Application.StarTerminal.State (Terminal(..), TerminalState(..))

main :: IO ()
main = do
  tId    <- TerminalId . SB . encodeUtf8 . pack <$> getEnv "STAR_TERMINAL_ID"
  pubkey <- decode'                             <$> getEnv "STAR_PUBLIC_KEY"
  zp     <- PublicHash   . decode 32            <$> getEnv "STAR_INIT_PUBLIC_HASH"
  zi     <- InternalHash . decode 32            <$> getEnv "STAR_INIT_INTERNAL_HASH"
  z0     <- B.encode     . decode 32            <$> getEnv "STAR_PUBLIC_SALT"
  seed   <- getStdGen
  let term = Terminal { _tId    = tId
                      , _pubkey = pubkey
                      , _zp0    = zp
                      , _zi0    = zi
                      , _z0     = z0
                      }
  statefulErrorServe site $ TerminalState def term seed

site :: StarTerm m => m ()
site =
    -- ifTop (formHandler) <|>
    route [ ("ballot/:ballotId/step/:stepId", method GET  showBallotStep)
          , ("ballot/:ballotId/step/:stepId", method POST recordBallotSelection)
          , ("ballot/:ballotId",              method GET  ballotHandler)
          , ("ballot/:ballotId/summary",      method GET  showSummary)
          , ("ballot/:ballotId/summary",      method POST finalize)
          , ("ballot/:ballotId/complete",     method GET  exitInstructions)
          ] <|>
    dir "static" (serveDirectory "static")

decode :: Int64 -> String -> SerializableBS
decode n s = if BS.length bs == n then SB bs else
  error $ "Length of input should be " ++ show n ++ " bytes."
  where
    bs = decode' s

decode' :: String -> ByteString
decode' = fst . B16.decode . encodeUtf8 . pack
