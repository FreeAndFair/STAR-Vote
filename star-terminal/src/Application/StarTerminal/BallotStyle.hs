{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.BallotStyle where

import           Data.Text (Text)

type BallotStyles = [(Text, BallotStyle)]

data BallotStyle = BallotStyle [Race]

data Race = Race
  { _rDescription :: Text
  , _rOptions :: [Option]
  }

data Option = Option
  { _oId :: Text
  , _oName :: Text
  , _oParty :: Maybe Text
  , _oOccupation :: Maybe Text
  }

bRace :: Int -> BallotStyle -> Maybe Race
bRace 0 (BallotStyle (r:_)) = Just r
bRace n (BallotStyle (_:rs)) = bRace (n - 1) (BallotStyle rs)
bRace _ _                    = Nothing
