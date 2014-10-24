{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Application.StarTerminal.State
Description : Types representating server-state that persists across requests

The web server maintains one value of type TerminalState in a TVar.
This value records information that is to be collected by the terminal,
and includes configuration parameters.

Configuration parameters are communicated to the web server via environment variables.
The mappings of environment variables are:

  * `STAR_TERMINAL_ID`        --> _tId
  * `STAR_PUBLIC_KEY`         --> _pubkey
  * `STAR_INIT_PUBLIC_HASH`   --> _zp0
  * `STAR_INIT_INTERNAL_HASH` --> _zi0
  * `STAR_PUBLIC_SALT`        --> _z0
  * `STAR_POST_VOTE_URL`      --> _postUrl

`STAR_TERMINAL_ID` may be any string.

`STAR_PUBLIC_KEY`, `STAR_INIT_PUBLIC_HASH`, `STAR_INIT_INTERNAL_HASH`,
`STAR_PUBLIC_SALT` must be encoded strings using base-16.  The hashes and salt
must each be 256 bits.
 -}
module Application.StarTerminal.State where

import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Application.Star.HashChain
import           Application.Star.Ballot
import           Application.Star.BallotStyle

data TerminalState = TerminalState
  {
    -- | All votes made on the terminal, kept in reverse-chronological order to
    -- make it easy to verify that each vote incorporates a hash of the
    -- previous.
    _recordedVotes :: [EncryptedRecord]
    -- | Mappings from ballot codes to ballot styles are transmitted to each
    -- terminal when voters check in at a polling place.
    -- Voters enter a code in a terminal to retrieve the appropriate ballot
    -- style.
  , _ballotCodes   :: Map BallotCode BallotStyle
  , _terminal      :: Terminal
  }

-- \ Records configuration paramaters that should not change while the terminal
-- is in operation.
data Terminal = Terminal
  { _tId     :: TerminalId    -- ^ unique identifier for the terminal
  , _pubkey  :: PublicKey     -- ^ issued by the election authority, used to encrypt votes
  , _zp0     :: PublicHash    -- ^ initial hash for the publicly viewable hash chain
  , _zi0     :: InternalHash  -- ^ initial hash for the non-public hash chain
  , _z0      :: ByteString    -- ^ public, random salt
  , _postUrl :: String        -- ^ The terminal posts each encrypted vote to this URL
  }

insertCode :: BallotCode -> BallotStyle -> TerminalState -> TerminalState
insertCode code style s = s { _ballotCodes = codes' }
  where
    codes  = _ballotCodes s
    codes' = Map.insert code style codes

lookupBallotStyle :: BallotCode -> TerminalState -> Maybe BallotStyle
lookupBallotStyle code s = Map.lookup code (_ballotCodes s)
