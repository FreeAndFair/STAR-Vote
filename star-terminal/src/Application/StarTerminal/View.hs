{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.View where

import           Data.List (foldl')
import           Data.Monoid ((<>), mempty)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (attribute)
import           Prelude hiding (div)

import           Application.StarTerminal.Ballot
import           Application.StarTerminal.BallotStyle
import           Application.StarTerminal.Localization

data NavLinks = NavLinks
  { _prev :: Maybe Text
  , _next :: Maybe Text
  , _index :: Maybe Text
  }

ballotStepView :: Translations -> NavLinks -> Race -> Maybe Selection -> Html
ballotStepView ts navLinks r s = withNav ts navLinks $
  div ! class_ "container" $ do
    div ! class_ "page-header" $ do
      h1 (toHtml (_rDescription r))
    H.form ! role "form" ! A.method "post" $ do
      foldl' (\h o -> h <> ballotOptionView ts s o) mempty (_rOptions r)
      H.button ! type_ "submit" ! class_ "btn btn-default" $ do
        (t "submit" ts)

ballotOptionView :: Translations -> Maybe Selection -> Option -> Html
ballotOptionView _ s o =
  div ! class_ "radio" $ do
    H.label $ do
      input ! type_ "radio" ! name "selection" ! value k ! isChecked
      H.span $ do
        toHtml (_oName o)
        whitespace
        maybe mempty (\party -> toHtml (T.concat ["(", party, ")"])) (_oParty o)
      br
      small ! class_ "text-muted" $ do
        maybe nbsp toHtml (_oOccupation o)
  where
    k = toValue (_oId o)
    isChecked = case s of
      Just s' -> if s' == _oId o then checked "checked" else mempty
      Nothing -> mempty

-- TODO: Serve our own jquery, ie shims.
page :: Text -> Html -> Html
page pageTitle pageContent = docTypeHtml ! lang "en" $ do
  H.head $ do
    meta ! charset "utf-8"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title (toHtml pageTitle)
    link ! href "/static/bootstrap-3.2.0-dist/css/bootstrap.min.css" ! rel "stylesheet"
    link ! href "/static/css/site.css" ! rel "stylesheet"
    ieShims
  body $ do
    pageContent
    script mempty ! src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
    script mempty ! src "/static/bootstrap-3.2.0-dist/js/bootstrap.min.js"

withNav :: Translations -> NavLinks -> Html -> Html
withNav ts navLinks c = navbar ts navLinks <> c

navbar :: Translations -> NavLinks -> Html
navbar ts navLinks =
  H.div ! class_ "navbar navbar-default navbar-fixed-top" ! role "navigation" $ H.div ! class_ "container" $ do
      maybe mempty (\url -> navLink "navbar-left" url $ do
          H.span mempty ! class_ "glyphicon glyphicon-chevron-left"
          whitespace
          t "previous_step" ts)
          (_prev navLinks)
      maybe mempty (\url -> navLink "navbar-right" url $ do
          t "next_step" ts
          whitespace
          H.span mempty ! class_ "glyphicon glyphicon-chevron-right")
          (_next navLinks)
      maybe mempty (\url -> navLink "center-block" url $ do
          t "show_progress" ts)
          (_index navLinks)

navLink :: Text -> Text -> Html -> Html
navLink classes url l =
  p ! class_ cs $ do
    a ! href (toValue url) ! class_ "btn btn-default" $ do
      l
  where
    cs = toValue (T.append "navbar-btn " classes)

role :: AttributeValue -> Attribute
role = attribute "role" " role=\""

ieShims :: Html
ieShims = preEscapedToHtml $ T.unlines
  [ "<!-- HTML5 Shim and Respond.js IE8 support of HTML5 elements and media queries -->"
  , "<!-- WARNING: Respond.js doesn't work if you view the page via file:// -->"
  , "<!--[if lt IE 9]>"
  , "  <script src=\"https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js\"></script>"
  , "  <script src=\"https://oss.maxcdn.com/respond/1.4.2/respond.min.js\"></script>"
  , "<![endif]-->"
  ]

t :: Text -> Translations -> Html
t k strings = toHtml (localize k strings)

whitespace :: Html
whitespace = toHtml (" " :: Text)

nbsp :: Html
nbsp = preEscapedToHtml ("&nbsp;" :: Text)
