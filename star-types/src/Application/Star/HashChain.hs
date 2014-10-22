{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , encryptRecord
  , internalHash
  , publicHash
  ) where

import           Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Binary (Binary)
import qualified Data.Binary as B
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Digest.Pure.SHA (bytestringDigest, sha256)
import           Data.List (foldl')
import           Data.Monoid (Monoid, mempty)
import           Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import           Application.Star.Ballot

newtype SerializableBS = SB ByteString
  deriving (Binary, Monoid)

instance ToJSON SerializableBS where
  toJSON (SB bs) = toJSON (decodeUtf8 (B64.encode bs))

instance FromJSON SerializableBS where
  parseJSON v = do
    txt <- parseJSON v
    case B64.decode (encodeUtf8 txt) of
      Right bs -> return $ SB bs
      Left err -> fail err

newtype BallotId = BallotId SerializableBS
  deriving (Binary, FromJSON, ToJSON)

newtype BallotCastingId = BallotCastingId SerializableBS
  deriving (Binary, FromJSON, ToJSON)

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
  , _pv   :: Proof Ballot
  , _cbid :: Encrypted [Hash RaceSelection]
  , _m    :: TerminalId
  , _zp   :: PublicHash
  , _zi   :: InternalHash
  }

$(deriveJSON defaultOptions ''EncryptedRecord)

encryptRecord :: PublicKey
              -> TerminalId
              -> BallotId
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

encryptBallot :: PublicKey -> Ballot -> (Encrypted Ballot, Proof Ballot)
encryptBallot k b = (Encrypted (SB (encrypt k b)), proof)
  where
    proof = Proof mempty -- TODO: not an actual proof

encryptRaces :: PublicKey -> BallotId -> [RaceSelection] -> Encrypted [Hash RaceSelection]
encryptRaces k bid rs = Encrypted $ foldl' (<||>) mempty (map (encryptRace k bid) rs)

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

encrypt :: Binary a => PublicKey -> a -> ByteString
encrypt _ = id . B.encode  -- TODO: Worst. Encryption. Scheme. Ever.

decrypt :: PrivateKey -> ByteString -> ByteString
decrypt _ = id

hash :: Binary a => a -> SerializableBS
hash = SB . bytestringDigest . sha256 . B.encode

(<||>) :: (Binary a, Binary b) => a -> b -> SerializableBS
x <||> y = SB $ BS.append (B.encode x) (B.encode y)
