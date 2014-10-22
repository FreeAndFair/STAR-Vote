{-# LANGUAGE OverloadedStrings #-}

module Application.StarTerminal.State where

import           Data.ByteString.Lazy (ByteString)
import           System.Random (StdGen)

import           Application.Star.HashChain

data TerminalState = TerminalState
  { _recordedVotes :: [EncryptedRecord]
  , _terminal      :: Terminal
  , _seed          :: StdGen
  }

data Terminal = Terminal
  { _tId    :: TerminalId
  , _pubkey :: PublicKey
  , _zp0    :: PublicHash
  , _zi0    :: InternalHash
  , _z0     :: ByteString
  }
