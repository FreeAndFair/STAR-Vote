{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.LinkHelper where

import           Data.Text (Text)
import qualified Data.Text as T

import           Application.Star.BallotStyle

stepUrl :: BallotStyleId -> RaceId -> Text
stepUrl bId rId = T.concat ["/ballot/", bId, "/step/", rId]

nextStepUrl :: BallotStyle -> Race -> Text
nextStepUrl style race = case nextRace style race of
  Just r  -> stepUrl (_bId style) (_rId r)
  Nothing -> summaryUrl style

firstStepUrl :: BallotStyle -> Text
firstStepUrl style = stepUrl (_bId style) (_rId (head (bRaces style)))

lastStepUrl :: BallotStyle -> Text
lastStepUrl style = stepUrl (_bId style) (_rId (last (bRaces style)))

progressUrl :: BallotStyle -> Maybe Race -> Text
progressUrl style _ = T.concat ["/ballot/", _bId style, "/progress"]

summaryUrl :: BallotStyle -> Text
summaryUrl style = T.concat ["/ballot/", _bId style, "/summary"]

exitInstructionsUrl :: BallotStyle -> Text
exitInstructionsUrl style = T.concat ["/ballot/", _bId style, "/complete"]
