{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.LinkHelper where

import           Control.Lens

import           Data.Monoid

import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           Application.Star.Ballot
import           Application.Star.BallotStyle

stepUrl :: BallotCode -> RaceId -> Text
stepUrl code rId' = T.concat ["/ballots/", T.pack (show code), "/step/", rId']

nextStepUrl :: BallotCode -> BallotStyle -> Race -> Text
nextStepUrl code style race =
  case nextRace style race of
    Just r  -> stepUrl code (view rId r)
    Nothing -> summaryUrl code

firstStepUrl :: BallotCode -> BallotStyle -> Text
firstStepUrl code style = stepUrl code $ view (bRaces . _head . rId) style

lastStepUrl :: BallotCode -> BallotStyle -> Text
lastStepUrl code style = stepUrl code $ view (bRaces . _last . rId) style

progressUrl :: BallotCode -> Maybe Race -> Text
progressUrl code _ = mconcat ["/ballots/", T.pack (show code), "/progress"]

summaryUrl :: BallotCode -> Text
summaryUrl code = mconcat ["/ballots/", T.pack (show code), "/summary"]

exitInstructionsUrl :: BallotId -> Text
exitInstructionsUrl (BallotId code) = mconcat ["/receipt/", code, "/"]

ballotReceiptUrl :: BallotId -> Text
ballotReceiptUrl (BallotId code) = mconcat ["/receipt/", code, "/print"]

stopStudyUrl :: Text
stopStudyUrl = "/study/stop"

aboutStudyUrl :: Text
aboutStudyUrl = "/study/about"

signInUrl :: Text
signInUrl = "/ballots"

portraitUrl :: Text -> Text
portraitUrl img = "/static/img/" <> img
