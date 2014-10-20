{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.BallotStyle where

import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T

type BallotId = Text
type RaceId = Text
type OptionId = Text
type BallotStyles = [(BallotStyle)]

type BallotKey = Text

data BallotStyle = BallotStyle
  { _bId :: BallotId
  , _bRaces :: [Race]
  }

data Race = Race
  { _rDescription :: Text
  , _rId :: RaceId
  , _rOptions :: [Option]
  }

data Option = Option
  { _oId :: OptionId
  , _oName :: Text
  , _oParty :: Maybe Text
  , _oOccupation :: Maybe Text
  }

lookup :: BallotId -> BallotStyles -> Maybe BallotStyle
lookup bId styles = safeHead (filter ((== bId) . _bId) styles)

bRace :: RaceId -> BallotStyle -> Maybe Race
bRace rId style = safeHead (filter ((== rId) . _rId) (_bRaces style))

bRaces :: BallotStyle -> [Race]
bRaces = _bRaces

nextRace :: BallotStyle -> Race -> Maybe Race
nextRace = incRace 1

prevRace :: BallotStyle -> Race -> Maybe Race
prevRace = incRace (-1)

incRace :: Int -> BallotStyle -> Race -> Maybe Race
incRace n style race = if idx + n < length races && idx + n >= 0 then
    Just (races !! (idx + n))
  else
    Nothing
  where
    races = bRaces style
    idx = fromMaybe (-1) $ List.findIndex ((== _rId race) . _rId) races


option :: Text -> Race -> Maybe Option
option optId race = safeHead (filter ((== optId) . _oId) (_rOptions race))

key :: BallotStyle -> Race -> BallotKey
key style race = key' (_bId style) (_rId race)

key' :: BallotId -> RaceId -> BallotKey
key' bId rId = T.concat [bId, "---", rId]

fromKey :: BallotKey -> Maybe (BallotId, RaceId)
fromKey t = params
  where
    parts = T.splitOn "---" t
    params = case parts of
      (bId:rId:_) -> Just (bId, rId)
      _           -> Nothing

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing
