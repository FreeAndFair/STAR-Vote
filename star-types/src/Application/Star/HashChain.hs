{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.Star.HashChain
  ( BallotId
  , BallotCastingId
  , Encrypted
  , EncryptedRecord (..)
  , InternalHash
  , PublicHash
  , TerminalId
  , encryptBallot
  , encryptRaces
  , internalHash
  , publicHash
  ) where

import           Data.Binary (Binary)
import qualified Data.Binary as B
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Digest.Pure.SHA (SHA256State, Digest, sha256)
import           Data.List (foldl')
import           Data.Monoid (mempty)

import           Application.Star.Ballot

newtype BallotId = BallotId ByteString
  deriving Binary

newtype BallotCastingId = BallotCastingId ByteString
  deriving Binary

newtype TerminalId = TerminalId ByteString
  deriving Binary

newtype Encrypted a = Encrypted ByteString
  deriving Binary

newtype PublicHash = PublicHash (Digest SHA256State)
  deriving Binary

newtype InternalHash = InternalHash (Digest SHA256State)
  deriving Binary

type Hash a = Digest SHA256State

data EncryptedRecord = EncryptedRecord
  { _bcid :: BallotCastingId
  , _cv   :: Encrypted Ballot
  , _pv   :: Proof Ballot
  , _cbid :: Encrypted [Hash RaceSelection]
  , _m    :: TerminalId
  , _zp   :: PublicHash
  , _zi   :: InternalHash
  }

encryptBallot :: PublicKey -> Ballot -> (Encrypted Ballot, Proof Ballot)
encryptBallot k b = (Encrypted (encrypt k b), proof)
  where
    proof = Proof mempty -- TODO: not an actual proof

encryptRaces :: PublicKey -> BallotId -> [RaceSelection] -> Encrypted [Hash RaceSelection]
encryptRaces k bid rs = Encrypted $ foldl' (<||>) mempty (map (encryptRace k bid) rs)

encryptRace :: PublicKey
            -> BallotId
            -> RaceSelection
            -> Encrypted (Hash RaceSelection)
encryptRace k bid r = Encrypted $ encrypt k (hash (bid <||> r))

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

encrypt :: Binary a => PublicKey -> a -> ByteString
encrypt _ = id . B.encode  -- TODO: Worst. Encryption. Scheme. Ever.

decrypt :: PrivateKey -> ByteString -> ByteString
decrypt _ = id

hash :: ByteString -> Digest SHA256State
hash = sha256

-- TODO:
newtype Proof a = Proof ByteString
  deriving Binary
newtype Ext a = Ext ByteString
  deriving Binary

(<||>) :: (Binary a, Binary b) => a -> b -> ByteString
x <||> y = BS.append (B.encode x) (B.encode y)
