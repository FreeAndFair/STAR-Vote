{-# LANGUAGE OverloadedStrings #-}

module Application.StarTerminal.State where

import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Application.Star.HashChain
import           Application.Star.Ballot
import           Application.Star.BallotStyle

data TerminalState = TerminalState
  { _recordedVotes :: [EncryptedRecord]
  , _ballotCodes   :: Map BallotCode BallotStyle
  , _terminal      :: Terminal
  }

data Terminal = Terminal
  { _tId     :: TerminalId
  , _pubkey  :: PublicKey
  , _zp0     :: PublicHash
  , _zi0     :: InternalHash
  , _z0      :: ByteString
  , _postUrl :: String
  }

insertCode :: BallotCode -> BallotStyle -> TerminalState -> TerminalState
insertCode code style s = s { _ballotCodes = codes' }
  where
    codes  = _ballotCodes s
    codes' = Map.insert code style codes

lookupBallotStyle :: BallotCode -> TerminalState -> Maybe BallotStyle
lookupBallotStyle code s = Map.lookup code (_ballotCodes s)
