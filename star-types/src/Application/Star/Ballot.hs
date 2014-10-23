{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.Star.Ballot where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Binary (Binary, get, put)
import           Data.Binary.Get (getRemainingLazyByteString)
import           Data.Binary.Put (putLazyByteString)
import           Data.Csv (DecodeOptions(..), EncodeOptions(..))
import qualified Data.Csv as CSV
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Vector ((!?))
import qualified Data.Vector as Vector

import           Application.Star.BallotStyle
import           Application.Star.Mod
import           Application.Star.SerializableBS

type Selection = Text

newtype BallotId = BallotId SerializableBS
  deriving (Binary, FromJSON, ToJSON)

newtype BallotCastingId = BallotCastingId SerializableBS
  deriving (Eq, Ord, Binary, Read, Show, FromJSON, ToJSON)

data BallotStatus = Unknown | Spoiled | Cast
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

newtype Ballot = Ballot { _bMap :: Map BallotKey Selection }
newtype RaceSelection = RaceSelection (BallotKey, Selection)

lookup :: Text -> Ballot -> Maybe Selection
lookup k (Ballot b) = Map.lookup k b

insert :: BallotKey -> Selection -> Ballot -> Ballot
insert k s (Ballot b) = Ballot (Map.insert k s b)

empty :: Ballot
empty = Ballot Map.empty

data HumanReadableLength
instance Bound HumanReadableLength where bound _ = 100000

type BallotCode = Mod HumanReadableLength

races :: Ballot -> [RaceSelection]
races (Ballot m) = map RaceSelection (Map.toAscList m)

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
