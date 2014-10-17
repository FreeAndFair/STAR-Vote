{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.View where

import           Data.List (foldl')
import           Data.Monoid ((<>), mempty)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
-- import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (attribute)
import           Prelude hiding (div)

import           Application.StarTerminal.Localization

formView :: Translations -> Html
formView ts = div ! class_ "container" $ do
  H.form ! role "form" $ do
    h1 (t "select_candidate" ts)
    foldl' (\h c -> h <> candidateView c) mempty gubCandidates

candidateView :: (Text, Text) -> Html
candidateView (k, c) =
  div ! class_ "radio" $ do
    H.label $ do
      input ! type_ "radio" ! name "candidate" ! value (toValue k)
      toHtml c

-- TODO: Serve our own jquery, ie shims.
page :: Text -> Html -> Html
page pageTitle pageContent = docTypeHtml ! lang "en" $ do
  H.head $ do
    meta ! charset "utf-8"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title (toHtml pageTitle)
    link ! href "static/bootstrap-3.2.0-dist/css/bootstrap.min.css" ! rel "stylesheet"
    link ! href "static/css/site.css" ! rel "stylesheet"
    ieShims
  body $ do
    pageContent
    script mempty ! src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
    script mempty ! src "static/bootstrap-3.2.0-dist/js/bootstrap.min.js"

navbar :: Translations -> Html
navbar ts =
  H.div ! class_ "navbar navbar-default navbar-fixed-top" ! role "navigation" $ H.div ! class_ "container" $ do
      button ! type_ "button" ! class_ "btn btn-default navbar-btn navbar-left" $ do
        H.span mempty ! class_ "glyphicon glyphicon-chevron-left"
        whitespace
        t "previous_step" ts
      button ! type_ "button" ! class_ "btn btn-default navbar-btn navbar-right" $ do
        t "next_step" ts
        whitespace
        H.span mempty ! class_ "glyphicon glyphicon-chevron-right"
      button ! type_ "button" ! class_ "btn btn-default navbar-btn center-block" $ do
        t "show_progress" ts

container :: Html -> Html
container pageContent =
  H.div ! class_ "container" $ do
    pageContent

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




gubCandidates :: [(Text, Text)]
gubCandidates =
  [ ("c1", "Aaron Auer (Con) Minister of the Gospel")
  , ("c2", "Tovia E Fornah (Non) Service")
  , ("c3", "Paul Grad (L) Investor")
  , ("c4", "Chris Henry (P)")
  , ("c5", "John Kitzhaber (Dem) Governor of Oregon")
  , ("c6", "Jason Levin (Grn) Cannabis Industry Professional")
  , ("c7", "Dennis Richardson (Rep) Businessman; State Representative")
  ]
