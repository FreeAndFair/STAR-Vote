{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Default (def)
import           Snap.Core
import           Snap.Util.FileServe
import           System.Random (getStdGen)

import           Application.Star.Util (statefulErrorServe)
import           Application.StarTerminal.Controller
import           Application.StarTerminal.State (TerminalState(..))

main :: IO ()
main = do
  seed <- getStdGen
  statefulErrorServe site $ TerminalState def seed

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
