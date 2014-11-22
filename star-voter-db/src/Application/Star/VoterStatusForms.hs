{-# LANGUAGE OverloadedStrings #-}
module Application.Star.VoterStatusForms where

import Application.Star.BallotStyle
import Application.Star.ID
import Application.Star.Voter

import Control.Applicative

import Data.Monoid

import qualified Data.Text as T

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive

import Text.Digestive.Blaze.Html5

nonEmpty :: Monad m => Form H.Html m T.Text
nonEmpty = check "Cannot be empty" (not . T.null) $ text Nothing

voterForm :: Monad m => Form H.Html m Voter
voterForm = Voter <$> "voterName" .: nonEmpty
                  <*> "voterAddress" .: nonEmpty

field :: T.Text -> View H.Html -> H.Html -> (T.Text -> View H.Html -> H.Html) -> H.Html
field ref view lbl widget = do
  H.div ! A.class_ ("row form-group" <> if err then " has-error" else "") $ do
    H.div ! A.class_ "col-md-1" $ label ref view lbl ! A.class_ "control-label"
    H.div ! A.class_ "col-md-5"  $ widget ref view ! A.class_ "form-control"
    H.div ! A.class_ "col-md-2 help-block" $ errorList ref view
  where err = not . null $ errors ref view

voterView :: View H.Html -> H.Html
voterView view =
  field "voterName"    view "Name"    inputText <>
  field "voterAddress" view "Address" (inputTextArea Nothing Nothing)


idForm :: Monad m => Form H.Html m (ID b)
idForm = "id" .: stringRead "Enter a value" Nothing

idView :: T.Text -> View H.Html -> H.Html
idView what view = field "id" view (H.toHtml what) inputText

ballotStyleForm :: Monad m => Form H.Html m BallotStyleId
ballotStyleForm = "ballotStyleId" .: nonEmpty

ballotStyleView :: View H.Html -> H.Html
ballotStyleView view = field "ballotStyleId" view "Ballot Style" inputText
