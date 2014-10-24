{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Application.Star.HashChain
Description : Functions and types for encrypting and hashing votes

The primary use of this module should be to call `encryptRecord`.
That function returns an `EncryptedRecord`,
which contains an encrypted vote along with public and internal hashes.
`EncryptedRecord` is configured to serialize to JSON for transmission over a wire.
This is the type that star-terminal records and transmits to a controller.
 -}
module Application.Star.HashChain
  ( Encrypted
  , EncryptedRecord (..)
  , InternalHash (..)
  , Proof
  , PublicHash (..)
  , PublicKey (..)
  , TerminalId (..)
  , encryptBallot
  , encryptRaces
  , encryptRecord
  , internalHash
  , publicHash
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Binary (Binary)
import qualified Data.Binary as B
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Digest.Pure.SHA (bytestringDigest, sha256)
import           Data.List (foldl')
import           Data.Monoid (mempty)

import           Application.Star.Ballot
import           Application.Star.SerializableBS

newtype TerminalId = TerminalId SerializableBS
  deriving (Binary, FromJSON, ToJSON)

newtype Encrypted a = Encrypted SerializableBS
  deriving (Binary, FromJSON, ToJSON)

newtype PublicHash = PublicHash SerializableBS
  deriving (Binary, FromJSON, ToJSON)

newtype InternalHash = InternalHash SerializableBS
  deriving (Binary, FromJSON, ToJSON)

type Hash a = SerializableBS

-- TODO:
newtype Proof a = Proof SerializableBS
  deriving (Binary, FromJSON, ToJSON)
newtype Ext a = Ext SerializableBS
  deriving (Binary, FromJSON, ToJSON)

data EncryptedRecord = EncryptedRecord
  { _bcid :: BallotCastingId
  , _cv   :: Encrypted Ballot
  , _pv   :: Proof Ballot  -- ^ NIZK proof of ballot correctness - not yet implemented
    -- | selections for each race, encrypted individually
  , _cbid :: Encrypted [Hash RaceSelection]
  , _m    :: TerminalId
  , _zp   :: PublicHash
  , _zi   :: InternalHash
  }

$(deriveJSON defaultOptions ''EncryptedRecord)

encryptRecord :: PublicKey   -- ^ public key issued by election authority; used to encrypt vote
              -> TerminalId  -- ^ unique ID of the terminal used to produce ballot
              -> BallotId    -- ^ unique, unpredictable ID for the given ballot
              -- ^ ID used to associate paper ballot with electronic record when
              -- the paper ballot is scanned at a ballot box
              -> BallotCastingId
              -> PublicHash
              -> InternalHash
              -> Ballot
              -> EncryptedRecord
encryptRecord k m bid bcid zp' zi' ballot = EncryptedRecord
  { _bcid = bcid
  , _cv   = cv
  , _pv   = pv
  , _cbid = cbid
  , _m    = m
  , _zp   = publicHash bcid extCv pv m zp'
  , _zi   = internalHash bcid cv pv cbid m zi'
  }
  where
    (cv, pv) = encryptBallot k ballot
    cbid     = encryptRaces k bid (races ballot)
    extCv = Ext mempty  -- TODO

encryptBallot :: PublicKey
              -> Ballot
              -> (Encrypted Ballot, Proof Ballot)
encryptBallot k b = (Encrypted (SB (encrypt k b)), proof)
  where
    proof = Proof mempty -- TODO: not an actual proof

encryptRaces :: PublicKey -> BallotId -> [RaceSelection] -> Encrypted [Hash RaceSelection]
encryptRaces k bid rs = Encrypted $ SB $ foldl' (<||>) mempty (map (encryptRace k bid) rs)

encryptRace :: PublicKey
            -> BallotId
            -> RaceSelection
            -> Encrypted (Hash RaceSelection)
encryptRace k bid r = Encrypted $ SB $ encrypt k (hash (bid <||> r))

publicHash :: BallotCastingId
           -> Ext (Encrypted Ballot)
           -> Proof Ballot
           -> TerminalId
           -> PublicHash
           -> PublicHash
publicHash bcid extCv pv m zp' =
  PublicHash $ hash (bcid <||> extCv <||> pv <||> m <||> zp')

internalHash :: BallotCastingId
             -> Encrypted Ballot
             -> Proof Ballot
             -> Encrypted [Hash RaceSelection]
             -> TerminalId
             -> InternalHash
             -> InternalHash
internalHash bcid cv pv cbid m zi' =
  InternalHash $ hash (bcid <||> cv <||> pv <||> cbid <||> m <||> zi')

type PublicKey  = ByteString
type PrivateKey = ByteString

-- | This is a placeholder - it does not actually implement a strong encryption
-- scheme.
encrypt :: Binary a => PublicKey -> a -> ByteString
encrypt _ = id . B.encode  -- TODO: Worst. Encryption. Ever.

-- | This is a placeholder - it does not actually implement a strong encryption
-- scheme.
decrypt :: PrivateKey -> ByteString -> ByteString
decrypt _ = id

hash :: Binary a => a -> SerializableBS
hash = SB . bytestringDigest . sha256 . B.encode

-- | Concatenates values to be hashed.
(<||>) :: (Binary a, Binary b) => a -> b -> ByteString
x <||> y = BS.append (B.encode x) (B.encode y)
