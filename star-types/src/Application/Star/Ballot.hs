{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|
Module      : Application.Star.Ballot
Description : Type for completed ballot

The `Ballot` represents a completed ballot,
as a mapping from race IDs (`BallotKey` values) to selections.

`Ballot` serializes to a `ByteString` so that it can be encrypted.
Serialization is achieved using CSV format (with tab-separated fields).
-}
module Application.Star.Ballot where

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Aeson.TH                   (defaultOptions, deriveJSON)
import           Data.Binary                     (Binary, get, put)
import qualified Data.Binary                     as B
import           Data.Binary.Get                 (getRemainingLazyByteString)
import           Data.Binary.Put                 (putLazyByteString)
import           Data.Csv                        (DecodeOptions (..),
                                                  EncodeOptions (..))
import qualified Data.Csv                        as CSV
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.SafeCopy                   (base, deriveSafeCopy)
import           Data.Text                       (Text)
import           Data.Text.Lazy                  (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           Data.Vector                     ((!?))
import qualified Data.Vector                     as Vector

import           Application.Star.BallotStyle
import           Application.Star.Mod
import           Application.Star.SerializableBS

type Selection = Text

newtype BallotId = BallotId Text
  deriving (FromJSON, ToJSON, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''BallotId)

newtype BallotCastingId = BallotCastingId Text
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, Typeable)
$(deriveSafeCopy 0 'base ''BallotCastingId)

data BallotStatus = Unknown | Spoiled | Cast
  deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)
$(deriveJSON defaultOptions ''BallotStatus)
$(deriveSafeCopy 0 'base ''BallotStatus)

newtype Ballot = Ballot { _bMap :: Map BallotKey Selection } deriving (Typeable)
$(deriveSafeCopy 0 'base ''Ballot)

newtype RaceSelection = RaceSelection (BallotKey, Selection)
$(deriveSafeCopy 0 'base ''RaceSelection)

lookup :: Text -> Ballot -> Maybe Selection
lookup k (Ballot b) = Map.lookup k b

insert :: BallotKey -> Selection -> Ballot -> Ballot
insert k s (Ballot b) = Ballot (Map.insert k s b)

empty :: Ballot
empty = Ballot Map.empty

data HumanReadableLength = HumanReadbleLength deriving Typeable
$(deriveSafeCopy 0 'base ''HumanReadableLength)
instance Bound HumanReadableLength where bound _ = 100000

type BallotCode = Mod HumanReadableLength

races :: Ballot -> [RaceSelection]
races (Ballot m) = map RaceSelection (Map.toAscList m)

instance Binary BallotId where
  put (BallotId txt) = putLazyByteString (encodeUtf8 (fromStrict (txt)))
  get = do
    bs <- getRemainingLazyByteString
    return $ BallotId (toStrict (decodeUtf8 bs))

instance Binary BallotCastingId where
  put (BallotCastingId txt) = putLazyByteString (encodeUtf8 (fromStrict (txt)))
  get = do
    bs <- getRemainingLazyByteString
    return $ BallotCastingId (toStrict (decodeUtf8 bs))

instance Binary Ballot where
  put (Ballot m) = putLazyByteString tsv
    where
      tsv = CSV.encodeWith csvEncOptions (Map.toAscList m)

  get = do
    bs <- getRemainingLazyByteString
    case CSV.decodeWith csvDecOptions CSV.NoHeader bs of
      Left err -> fail err
      Right (vec) -> return $ Ballot (Map.fromList (Vector.toList vec))

instance Binary RaceSelection where
  put (RaceSelection p) = putLazyByteString tsv
    where
      tsv = CSV.encodeWith csvEncOptions [p]

  get = do
    bs <- getRemainingLazyByteString
    case CSV.decodeWith csvDecOptions CSV.NoHeader bs of
      Left err -> fail err
      Right (vec) -> case vec !? 0 of
        Just h -> return $ RaceSelection h
        Nothing -> fail "empty tsv"

csvEncOptions :: EncodeOptions
csvEncOptions = CSV.defaultEncodeOptions
  { encDelimiter = 9  -- Tab
  , encIncludeHeader = False
  }

csvDecOptions :: DecodeOptions
csvDecOptions = CSV.defaultDecodeOptions
  { decDelimiter = encDelimiter csvEncOptions
  }
