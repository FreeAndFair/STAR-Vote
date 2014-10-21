{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}
module Application.Star.Ballot where

import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Text (Text)

import           Application.Star.BallotStyle
import           Application.Star.Mod

type Selection = Text

data Ballot = Ballot { _bMap :: Map BallotKey Selection }

lookup :: Text -> Ballot -> Maybe Selection
lookup k (Ballot b) = Map.lookup k b

insert :: BallotKey -> Selection -> Ballot -> Ballot
insert k s (Ballot b) = Ballot (Map.insert k s b)

empty :: Ballot
empty = Ballot Map.empty

data HumanReadableLength
instance Bound HumanReadableLength where bound _ = 100000

type BallotCode = Mod HumanReadableLength
