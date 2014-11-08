{-# LANGUAGE OverloadedStrings #-}
module Application.Star.VoterStatusForms where

import Application.Star.BallotStyle
import Application.Star.ID
import Application.Star.Voter

import Control.Applicative

import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H

import Text.Digestive

import Text.Digestive.Blaze.Html5

voterForm :: Monad m => Form H.Html m Voter
voterForm = Voter <$> "voterName" .: nonEmpty
                  <*> "voterAddress" .: nonEmpty
  where nonEmpty = check "Cannot be empty" (not . T.null) $ text Nothing


voterView :: View H.Html -> H.Html
voterView view = do
  errorList "voterName" view
  label     "voterName" view "Name"
  inputText "voterName" view
  H.br

  errorList                     "voterAddress" view
  label                         "voterAddress" view "Address"
  inputTextArea Nothing Nothing "voterAddress" view



idForm :: Monad m => Form H.Html m (ID b)
idForm = "id" .: stringRead "Enter a value" Nothing

idView :: T.Text -> View H.Html -> H.Html
idView what view = do
  errorList "id" view
  label     "id" view (H.toHtml what)
  inputText "id" view

ballotStyleForm :: Monad m => Form H.Html m BallotStyleId
ballotStyleForm = T.pack <$> "ballotStyleId" .: string Nothing

ballotStyleView :: View H.Html -> H.Html
ballotStyleView view = do
  errorList "ballotStyleId" view
  label     "ballotStyleId" view "Ballot Style"
  inputText "ballotStyleId" view
