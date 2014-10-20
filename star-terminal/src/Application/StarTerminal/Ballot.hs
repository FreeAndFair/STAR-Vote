{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.Ballot where

import           Control.Applicative ((<$>))
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import           Numeric (readDec)

import           Application.StarTerminal.BallotStyle

type BallotKey = Text
type Selection = Text

data Ballot = Ballot { _bMap :: Map BallotKey Selection }

lookup :: Text -> Ballot -> Maybe Selection
lookup k (Ballot b) = Map.lookup k b

insert :: Text -> Selection -> Ballot -> Ballot
insert k s (Ballot b) = Ballot (Map.insert k s b)

key :: BallotId -> Int -> Text
key bId idx = T.concat [bId, "---", pack (show idx)]

fromKey :: Text -> Maybe (BallotId, Int)
fromKey t = ((,) bId) <$> parseInt idx
  where
    (bId:idx:_) = T.splitOn "---" t

parseInt :: Text -> Maybe Int
parseInt = safeHead . map fst . readDec . unpack
  where
    safeHead (x:_) = Just x
    safeHead []     = Nothing
