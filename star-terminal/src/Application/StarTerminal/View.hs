{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Application.StarTerminal.View
Description : HTML views
 -}
module Application.StarTerminal.View where

import           Control.Lens
import           Control.Monad
import           Data.ByteString                       (ByteString)
import           Data.List                             (foldl')
import           Data.Maybe                            (isJust)
import           Data.Monoid                           (mempty, (<>))
import           Data.String
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Prelude                               hiding (div)
import           Text.Blaze.Html5                      hiding (code)
import qualified Text.Blaze.Html5                      as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes           as A
import           Text.Blaze.Internal                   (attribute)

import           Application.Star.Ballot
import qualified Application.Star.Ballot               as Ballot
import           Application.Star.BallotStyle
import qualified Application.Star.BallotStyle          as BS
import           Application.StarTerminal.LinkHelper
import           Application.StarTerminal.Localization

-- | Defines URLs for links in the navigation bar when viewing ballot steps.
-- If no link is given, the corresponding button is not drawn.
data NavLinks = NavLinks
  { _prev  :: Maybe Text   -- ^ "previous step" button URL
  , _next  :: Maybe Text   -- ^ "next step" button URL
  , _index :: Maybe Text  -- ^ "show progress" button URL
  }

-- function(){} looks stupid, but works around a bug in old Safari versions
-- where clicking on a label wouldn't activate the associated widget.
labelEmptyOnclick :: Html -> Html
labelEmptyOnclick = H.label ! onclick "function(){};"

-- | Page that prompts voter to enter a ballot code.
codeEntryView :: Translations -> Html
codeEntryView ts =
  div ! class_ "container" $ do
    div ! class_ "page-header" $ do
      h1 (t "enter_ballot_code" ts)
    H.form ! role "form" ! A.method "get" $ do
      labelEmptyOnclick $ do
        t "ballot_code_label" ts
        whitespace
        input ! type_ "text" ! name "code"
      H.button ! type_ "submit" ! class_ "btn btn-default" $ do
        (t "submit" ts)

-- | Displays a single race, prompts voter to make a selection.
ballotStepView :: Translations -> NavLinks -> BallotStyle -> Race -> Maybe Selection -> Html
ballotStepView ts navLinks bStyle r s = withNav ts navLinks $
  div ! class_ "container content" $ do
    div ! class_ "page-header" $ do
      h1 (toHtml (_rDescription r))
      p (t "select_candidate" ts)
    H.form ! role "form" ! A.method "post" $ do
      div ! class_ "radio" $
        foldl' (\h o -> h <> ballotOptionView ts s o) mempty (_rOptions r)
      input ! type_ "hidden" ! name "race-key" ! value (toValue (key bStyle r))
      noscript $ do
        H.button ! type_ "submit" ! class_ "btn btn-default" $ do
          (t "submit" ts)

ballotOptionView :: Translations -> Maybe Selection -> Option -> Html
ballotOptionView ts s o = do
  input ! type_ "radio" ! name "selection" ! class_ "ballot-option unlabelled" ! value k ! isChecked ! A.id k
  labelEmptyOnclick ! for k ! class_ "ballot-option" $ do
    div ! class_ "portrait" $ do
      maybe noPortrait portrait (_oImg o)
    H.span $ do
      selectionDescription o
    br
    small ! class_ "candidate-occupation text-muted" $ do
      maybe nbsp toHtml (_oOccupation o)
  br
  where
    portrait imgName = img ! alt (toValue (localize "portrait"    ts)) ! src (toValue (portraitUrl imgName          ))
    noPortrait       = img ! alt (toValue (localize "no_portrait" ts)) ! src (toValue (portraitUrl "no_portrait.jpg"))
    k = toValue (_oId o)
    isChecked = case s of
      Just s' -> if s' == _oId o then checked "checked" else mempty
      Nothing -> mempty

view404 :: Html
view404 = "Not found"

internalErrorView :: Html
internalErrorView = "Internal error"

selectionDescription :: Option -> Html
selectionDescription o = do
  H.span ! class_ "candidate-name" $ toHtml (_oName o)
  whitespace
  maybe mempty (\party -> H.span ! class_ "party" $ toHtml party) (_oParty o)

-- | After ballot steps are complete,
-- displays summary of selections that the voter has made.
summaryView :: Translations -> BallotCode -> BallotStyle -> Ballot -> Html
summaryView ts code bStyle ballot =
  H.form ! method "post" $ do
    div ! class_ "container content" $ do
      div ! class_ "page-header" $ do
        h1 (t "summary" ts)
        p (t "summary_instructions" ts)
      foldl' (\h race -> h <> summaryItemView ts code bStyle race ballot)
        mempty (view bRaces bStyle)
    navSummary ts code bStyle

summaryItemView :: Translations -> BallotCode -> BallotStyle -> Race -> Ballot -> Html
summaryItemView _ code bStyle race ballot =
  div ! class_ (toValue (T.append "summary-item" bgClass)) $ do
    p ! class_ "item-title text-left" $ do
      a ! href (toValue (stepUrl code rId)) $ do
        toHtml itemTitle
    p ! class_ "item-selection text-right" $ do
      case mOpt of
        Just opt -> selectionDescription opt
        Nothing  -> preEscapedToHtml ("&mdash;" :: Text)
  where
    itemTitle = _rDescription race
    selection = Ballot.lookup (key bStyle race) ballot
    mOpt = selection >>= \s -> BS.option s race
    bgClass = if isJust mOpt then "" else " bg-warning"
    rId = _rId race

-- | Message that is shown after a ballot has been finalized.
printReceiptView :: Text -> Translations -> Html
printReceiptView url ts =
  div ! class_ "container" $ do
    div ! class_ "page-header" $ do
      h1 (t "successful_vote" ts)
    p (t "collect_ballot_and_receipt" ts)

studyHeader :: Translations -> Text -> Html
studyHeader ts msg = div ! class_ "page-header study-header" $ do
  h1 (t msg ts)

welcomeView :: Translations -> Html
welcomeView ts =
  div ! class_ "container" $ do
    studyHeader ts "welcome_to_study"
    mapM_ (p . toHtml) . T.lines $ localize "study_description" ts
    bottomNav $ do
      navLinkLeft  stopStudyUrl  (t "not_ready_to_begin" ts)
      navLinkRight aboutStudyUrl (t "ready_to_begin"     ts)

aboutView :: Translations -> Html
aboutView ts =
  div ! class_ "container" $ do
    studyHeader ts "about_the_prototype"
    p (t "special_feature_intro" ts)
    ul . mapM_ (li . toHtml) . T.lines $ localize "special_feature_bullets" ts
    p (t "special_feature_outro" ts)
    bottomNav $ do
      stopLinkLeft ts
      navLinkRight "/ballots" (t "proceed" ts)

signInView :: Translations -> Html
signInView ts =
  div ! class_ "container" $ do
    studyHeader ts "sign_in"
    p (t "enter_email_code" ts)
    H.form ! role "form" ! A.method "get" $ do
      input ! type_ "text" ! name "code"
      whitespace
      H.button ! type_ "submit" ! class_ "btn btn-default" $ do
        (t "sign_in_button" ts)
    bottomNav $ do
      stopLinkLeft ts

ballotInstructionsView :: Translations -> Text -> Html
ballotInstructionsView ts url =
  div ! class_ "container" $ do
    studyHeader ts "filling_out_your_ballot"
    p  (t "mock_election"           ts)
    p  (t "verify_your_vote"        ts)
    bottomNav $ do
      stopLinkLeft ts
      navLinkRight url (t "proceed" ts)

completionView :: Translations -> BallotCastingId -> Html
completionView ts bcid =
  div ! class_ "container" $ do
    studyHeader ts "ballot_complete"
    p  (t "congratulations" ts)
    p  (t "cast_or_spoil"   ts)
    H.form ! role "form" ! A.method "post" ! action "/cast" $ do
      input ! type_ "hidden" ! name "bcid" ! value (toValue (show bcid))
      div ! class_ "text-center" $ do
        button ! class_ "btn btn-default navbar-btn"
               ! type_ "submit"
               ! name "action" ! value "cast"
               $ t "cast" ts
      div ! class_ "text-center" $ do
        button ! class_ "btn btn-default navbar-btn"
               ! type_ "submit"
               ! name "action" ! value "spoil"
               $ t "spoil" ts
    bottomNav $ do
      stopLinkLeft ts

castCompletedView :: Translations -> Html
castCompletedView ts =
  div ! class_ "container" $ do
    studyHeader ts "you_voted"
    p  (t "thank_you" ts)
    p  (t "answer_questions" ts)

spoilCompletedView :: Translations -> Html
spoilCompletedView ts =
  div ! class_ "container" $ do
    studyHeader ts "spoiled_ballot"
    p  (t "spoiled_explanation" ts)
    bottomNav $ do
      navLinkLeft stopStudyUrl (t "exit_study" ts)
      navLinkRight signInUrl (t "another_ballot" ts)

stopView :: Translations -> Html
stopView ts =
  div ! class_ "container" $ do
    studyHeader ts "thank_you_for_participation"
    p  (t "why_stop" ts)
    H.form ! role "form" ! A.method "post" $ do
      forM_ reasons $ \(ix, reason) -> div $ do
        labelEmptyOnclick $ do
          input ! type_ "checkbox"
                ! name ("reason" <> fromString (show ix))
          whitespace
          toHtml reason
      div $ do
        labelEmptyOnclick $ do
          input ! type_ "checkbox"
                ! name "other"
          whitespace
          t "other_reason_for_stopping" ts
        whitespace
        input ! type_ "text"
              ! name "other_reason"
      button ! class_ "btn btn-default navbar-btn navbar-left" ! type_ "submit" $ do
        t "submit" ts
  where
  reasons = zip [0..] (T.lines (localize "reasons_for_stopping" ts))

feedbackView :: Translations -> Html
feedbackView ts =
  div ! class_ "container" $ do
    studyHeader ts "feedback_is_nice"

stopLinkLeft :: Translations -> Html
stopLinkLeft ts = navLinkLeft stopStudyUrl (t "stop" ts)

-- | Page layout -
-- produces markup that appears on every page.
page :: AttributeValue -> Text -> Html -> Html
page css pageTitle pageContent = docTypeHtml ! lang "en" $ do
  H.head $ do
    meta ! charset "utf-8"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title (toHtml pageTitle)
    link ! href "/static/bootstrap-3.2.0-dist/css/bootstrap.min.css" ! rel "stylesheet"
    link ! href css ! rel "stylesheet"
    ieShims
  body $ do
    pageContent
    script mempty ! src "/static/js/site.js"

withNav :: Translations -> NavLinks -> Html -> Html
withNav ts navLinks c = c <> navbar ts navLinks

navbar :: Translations -> NavLinks -> Html
navbar ts navLinks =
  bottomNav $ do
    maybe mempty (\url -> navLinkLeft   url (t "previous_step" ts)) (_prev  navLinks)
    maybe mempty (\url -> navLinkRight  url (t "next_step"     ts)) (_next  navLinks)
    maybe mempty (\url -> navLinkCenter url (t "show_progress" ts)) (_index navLinks)

navSummary :: Translations -> BallotCode -> BallotStyle -> Html
navSummary ts code bStyle =
  bottomNav $ do
    navLinkLeft (lastStepUrl code bStyle) (t "previous_step" ts)
    button ! class_ "btn btn-default navbar-btn navbar-right" ! type_ "submit" $ do
      t "print_ballot" ts
    -- navLink "navbar-left" (progressUrl code Nothing) $ do
    --   t "show_progress" ts

bottomNav :: Html -> Html
bottomNav c =
  H.div ! class_ "navbar navbar-default navbar-fixed-bottom" ! role "navigation" $ H.div ! class_ "container" $ c

navLink :: Text -> Text -> Html -> Html
navLink classes url l =
  p ! class_ cs $ do
    a ! href (toValue url) ! class_ "btn btn-default" $ do
      l
  where
    cs = toValue (T.append "navbar-btn " classes)

navLinkLeft :: Text -> Html -> Html
navLinkLeft url label = navLink "navbar-left" url  $ do
  H.span mempty ! class_ "glyphicon glyphicon-chevron-left"
  whitespace
  label

navLinkRight :: Text -> Html -> Html
navLinkRight url label = navLink "navbar-right" url $ do
  label
  whitespace
  H.span mempty ! class_ "glyphicon glyphicon-chevron-right"

navLinkCenter :: Text -> Html -> Html
navLinkCenter url label = navLink "center-block" url label

role :: AttributeValue -> Attribute
role = attribute "role" " role=\""

-- TODO: Serve our own IE shims.
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

