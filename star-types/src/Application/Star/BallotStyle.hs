{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

{-|
Module      : Application.Star.BallotStyle
Description : Type for ballot templates

The `BallotStyle` type represents a ballot that is waiting to be filled out.
It consists of an identifier and a list of races,
where each race includes a list of candidates.
-}
module Application.Star.BallotStyle where

import Control.Lens

import qualified Data.List     as List
import           Data.Maybe    (fromMaybe)
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Typeable

type BallotStyleId = Text
type RaceId = Text
type OptionId = Text

type BallotKey = Text

data Option = Option
  { _oId         :: OptionId
  , _oName       :: Text
  , _oParty      :: Maybe Text
  , _oOccupation :: Maybe Text
  }
  deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''Option)
$(makeLenses ''Option)

data Race = Race
  { _rDescription :: Text
  , _rId          :: RaceId
  , _rOptions     :: [Option]
  }
  deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''Race)
$(makeLenses ''Race)

data BallotStyle = BallotStyle
  { _bId    :: BallotStyleId
  , _bRaces :: [Race]
  }
  deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''BallotStyle)
$(makeLenses ''BallotStyle)

type BallotStyles = [(BallotStyle)]

lookup :: BallotStyleId -> BallotStyles -> Maybe BallotStyle
lookup bId styles = safeHead (filter ((== bId) . _bId) styles)

bRace :: RaceId -> BallotStyle -> Maybe Race
bRace rId style = safeHead (filter ((== rId) . _rId) (_bRaces style))


-- | Given a ballot style and a race,
-- returns the next race defined by the ballot style.
nextRace :: BallotStyle -> Race -> Maybe Race
nextRace = incRace 1

-- | Given a ballot style and a race,
-- returns the previous race defined by the ballot style.
prevRace :: BallotStyle -> Race -> Maybe Race
prevRace = incRace (-1)

incRace :: Int -> BallotStyle -> Race -> Maybe Race
incRace n style race = if idx + n < length races && idx + n >= 0 then
    Just (races !! (idx + n))
  else
    Nothing
  where
    races = view bRaces style
    idx = fromMaybe (-1) $ List.findIndex ((== _rId race) . _rId) races


option :: Text -> Race -> Maybe Option
option optId race = safeHead (filter ((== optId) . _oId) (_rOptions race))

-- | Produces a key suitable for uniquely identifying a race in a given election.
-- `Ballot` values use keys produced by this function.
key :: BallotStyle -> Race -> BallotKey
key style race = key' (_bId style) (_rId race)

-- | Variant of `key` that takes a ballot style ID instead of a ballot style
-- value
key' :: BallotStyleId -> RaceId -> BallotKey
key' bId rId = T.concat [bId, "---", rId]

fromKey :: BallotKey -> Maybe (BallotStyleId, RaceId)
fromKey t = params
  where
    parts = T.splitOn "---" t
    params = case parts of
      (bId:rId:_) -> Just (bId, rId)
      _           -> Nothing

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing
