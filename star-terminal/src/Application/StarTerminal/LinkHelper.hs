{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.LinkHelper where

import           Control.Lens

import           Data.Monoid

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.Blaze                   (AttributeValue, toValue)

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

exitInstructionsUrl :: BallotId -> BallotCastingId -> Text
exitInstructionsUrl (BallotId code) (BallotCastingId bcid)
  = mconcat ["/receipt/", code, "/", bcid]

ballotReceiptUrl :: BallotId -> Text
ballotReceiptUrl (BallotId code) = mconcat ["/receipt/", code, "/print"]

ballotUrl :: BallotCastingId -> Text
ballotUrl (BallotCastingId bcid) = mconcat ["/ballots/", bcid, "/paper.pdf"]

stopStudyUrl :: Text
stopStudyUrl = "/study/stop"

aboutStudyUrl :: Text
aboutStudyUrl = "/study/about"

signInUrl :: Text
signInUrl = "/ballots"

portraitUrl :: Text -> Text
portraitUrl img = "/static/img/" <> img

defaultStylesheet :: AttributeValue
defaultStylesheet = stylesheet "site"

stylesheet :: Text -> AttributeValue
stylesheet s = toValue ("/static/css/" <> s <> ".css")
