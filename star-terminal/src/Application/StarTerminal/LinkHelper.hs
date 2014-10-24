{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.LinkHelper where

import           Data.Text (Text)
import qualified Data.Text as T

import           Application.Star.Ballot
import           Application.Star.BallotStyle

stepUrl :: BallotCode -> RaceId -> Text
stepUrl code rId = T.concat ["/ballots/", T.pack (show code), "/step/", rId]

nextStepUrl :: BallotCode -> BallotStyle -> Race -> Text
nextStepUrl code style race = case nextRace style race of
  Just r  -> stepUrl code (_rId r)
  Nothing -> summaryUrl code

firstStepUrl :: BallotCode -> BallotStyle -> Text
firstStepUrl code style = stepUrl code (_rId (head (bRaces style)))

lastStepUrl :: BallotCode -> BallotStyle -> Text
lastStepUrl code style = stepUrl code (_rId (last (bRaces style)))

progressUrl :: BallotCode -> Maybe Race -> Text
progressUrl code _ = T.concat ["/ballots/", T.pack (show code), "/progress"]

summaryUrl :: BallotCode -> Text
summaryUrl code = T.concat ["/ballots/", T.pack (show code), "/summary"]

exitInstructionsUrl :: BallotCode -> Text
exitInstructionsUrl code = T.concat ["/ballots/", T.pack (show code), "/complete"]
