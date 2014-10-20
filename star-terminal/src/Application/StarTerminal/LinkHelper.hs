{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.LinkHelper where

import           Data.Text (Text)
import qualified Data.Text as T

import           Application.StarTerminal.BallotStyle

stepUrl :: BallotId -> RaceId -> Text
stepUrl bId rId = T.concat ["/ballot/", bId, "/step/", rId]

nextStepUrl :: BallotStyle -> Race -> Text
nextStepUrl style race = case nextRace style race of
  Just r  -> stepUrl (_bId style) (_rId r)
  Nothing -> summaryUrl style

firstStepUrl :: BallotStyle -> Text
firstStepUrl style = stepUrl (_bId style) (_rId (head (bRaces style)))

summaryUrl :: BallotStyle -> Text
summaryUrl style = T.concat ["/ballot/", _bId style, "/summary"]
