{-# LANGUAGE OverloadedStrings #-}

module Application.StarTerminal.State where

import           System.Random (StdGen)

import           Application.Star.HashChain

data TerminalState = TerminalState
  { _recordedVotes :: [EncryptedRecord]
  , _seed          :: StdGen
  }
