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

import           Application.StarTerminal.Localization

formView :: Translations -> Html
formView ts = div ! class_ "container" $ do
  H.form ! role "form" $ do
    h2 (t "select_candidate" ts)
    ul $ do
      foldl' (\h c -> h <> candidateView c) mempty gubCandidates

candidateView :: (Text, Text) -> Html
candidateView (k, c) = li ! class_ "candidate" $ do
  H.label ! for l $ do
    toHtml c
  input ! type_ "radio" ! name "candidate" ! value (toValue k) ! A.id l
  where
    l = toValue (T.append "candidate_" k)

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

navbar :: Html
navbar =
  H.div ! class_ "navbar navbar-default navbar-fixed-top" ! role "navigation" $ H.div ! class_ "container" $ do
      H.div ! class_ "navbar-header" $ do
          button ! type_ "button" ! class_ "navbar-toggle collapsed" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-collapse" $ do
              H.span ! class_ "sr-only" $ "Toggle navigation"
              H.span ! class_ "icon-bar" $ mempty
              H.span ! class_ "icon-bar" $ mempty
              H.span ! class_ "icon-bar" $ mempty
          a ! class_ "navbar-brand" ! href "#" $ "Project name"
      H.div ! class_ "navbar-collapse collapse" $ do
          ul ! class_ "nav navbar-nav" $ do
              li ! class_ "active" $ a ! href "#" $ "Home"
              li $ a ! href "#about" $ "About"
              li $ a ! href "#contact" $ "Contact"
              li ! class_ "dropdown" $ do
                  a ! href "#" ! class_ "dropdown-toggle" ! dataAttribute "toggle" "dropdown" $ do
                      "Dropdown"
                      H.span ! class_ "caret" $ mempty
                  ul ! class_ "dropdown-menu" ! role "menu" $ do
                      li $ a ! href "#" $ "Action"
                      li $ a ! href "#" $ "Another action"
                      li $ a ! href "#" $ "Something else here"
                      li ! class_ "divider" $ mempty
                      li ! class_ "dropdown-header" $ "Nav header"
                      li $ a ! href "#" $ "Separated link"
                      li $ a ! href "#" $ "One more separated link"
          ul ! class_ "nav navbar-nav navbar-right" $ do
              li $ a ! href "../navbar/" $ "Default"
              li $ a ! href "../navbar-static-top/" $ "Static top"
              li ! class_ "active" $ a ! href "./" $ "Fixed top"

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
