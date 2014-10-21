{-# LANGUAGE OverloadedStrings #-}
module Application.Star.Ballot where

import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Text (Text)

import           Application.Star.BallotStyle

type Selection = Text

data Ballot = Ballot { _bMap :: Map BallotKey Selection }

lookup :: Text -> Ballot -> Maybe Selection
lookup k (Ballot b) = Map.lookup k b

insert :: BallotKey -> Selection -> Ballot -> Ballot
insert k s (Ballot b) = Ballot (Map.insert k s b)

empty :: Ballot
empty = Ballot Map.empty
