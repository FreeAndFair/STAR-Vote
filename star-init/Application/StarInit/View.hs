{-# LANGUAGE OverloadedStrings #-}

module Application.StarInit.View where

import Prelude hiding (div)

import Data.Monoid
import Data.String

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

getTerminalPublicKeyView :: Html
getTerminalPublicKeyView = do
  p "Enter the election public key below."
  H.form ! method "post" $ do
    textarea ! name "publickey" ! width "40" ! height "5" $ ""
    br
    button $ "Submit"

terminalStartedView :: String -> Html
terminalStartedView port = do
  "Terminal started on port " <> fromString port <> "."

instructionsView :: Html
instructionsView = do
  h2 "Configuring the election"
  ol $ do
    li $ do
      a ! href "/subsystems/BB/start" $ do
        "Start the write-only bulletin board."
    li $ do
      a ! href "localhost:8000/reset" $ do
        "Clear the bulletin board."
    li $ do
      a ! href "/subsystems/LEO/start" $ do
        "Start the pre-election setup servers."
    li $ do
      a ! href "localhost:8001/register.html" $ do
        "Register the key generator server with the bulletin board."
    li $ do
      a ! href "localhost:8001/initialize.html" $ do
        "Generate keys for the local election officials."
    li $ do
      a ! href "localhost:8002/initialize" $ do
        "Initialize the voter database."
    li $ do
      a ! href "/subsystems/controller/start" $ do
        "Start the polling booth controller."
    li $ do
      a ! href "localhost:8003/clear" $ do
        "Clear the polling booth controller."
    li $ do
      a ! href "/subsystems/terminal/start" $ do
        "Start the voting terminal."
  h2 "Running the election"
  ul $ do
    li $ do
      strong "Check-in station: "
      a ! href "localhost:8002" $ "Look people up by name."
    li $ do
      strong "Ballot claim station: "
      a ! href "localhost:8003/generateCode" $ "Generate voting codes by scanning bar codes."
    li $ do
      strong "Voting terminal: "
      a ! href "localhost:8004" $ "Enter a voting code from the ballot claim station."
    li $ do
      strong "Vote submission: "
      a ! href "localhost:8003/cast" $ "Cast"
      " or "
      a ! href "localhost:8003/spoil" $ "spoil"
      " a filled-out ballot by scanning the \"Casting ID\"."
  h2 "Finalizing the election"
  p $ do
    a ! href "localhost:8003/tally" $ "Compute the tally."
