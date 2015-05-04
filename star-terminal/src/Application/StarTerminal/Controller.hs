{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Application.StarTerminal.Controller
Description : Defines web server actions

Defines handlers, which are called from @Main@.
Handles state changes,
invokes functions in 'Application.StarTerminal.View' to render pages.

The terminal must be given ballot code mappings.
Voters will enter codes to get access to appropriate ballot styles.
Code mappings are communicated to the terminal via POST requests,
which are handled by 'recordBallotStyleCode'.

The first page that a voter should see is handled by 'askForBallotCode'.

Filling out a ballot involves multiple steps,
where each step is a separate page that prompts the user to make a selection in
a single race.
'showBallotStep' handles rendering those pages.
It requires a ballot code as a parameter.
This ensures that only a voter with a valid code can view a ballot.

At each step, candidate selections are recorded in browser cookies.
A cookie is written (or updated) in javascript as soon as the voter makes
a selection by toggling a radio button.
But in case javascript is not enabled,
the voter may make a form submission, which is handled by 'recordBallotSelection'.
The handler sets a cookie as part of a redirect response.
To enable form submission, a \"Submit\" button is included in ballot step pages in
a @\<noscript\>@ tag.

The voter may navigate forward or backward arbitrarily while filling out the
ballot.

Upon completing or skipping all ballot steps, the voter is presented with
a summary of his/her selections.
This page is produced by 'showSummary'.
Upon selecting \"done with this ballot\", a POST request is made,
which is handled by 'finalize'.
At this point the vote is encrypted and hashed.
The hashes and encrypted vote are stored in the terminal's internal state,
and are also transmitted via HTTP POST to a controller machine.

'finalize' redirects to a page handled by 'exitInstructions'.
This page indicates that in a future version,
the terminal will actually print the voter's ballot at this point.
It also reminds the voter to take the ballot to a ballot box,
and to keep the printed receipt.
 -}
module Application.StarTerminal.Controller where

import           Control.Applicative                   (liftA2, liftA3, (<$>))
import           Control.Lens
import           Control.Monad                         (join, when)
import           Control.Monad.Except                  (MonadError)
import           Control.Monad.IO.Class                (liftIO)
import           Data.Acid                             (AcidState, query, update)
import qualified Data.Aeson                            as JSON
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Char8                 as Char
import           Data.List                             (foldl')
import           Data.Maybe                            (catMaybes, fromJust,
                                                        isNothing)
import           Data.Monoid                           ((<>))
import           Data.Text                             (Text, pack)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (decodeUtf8With,
                                                        encodeUtf8)
import           Data.Text.Encoding.Error              (ignore)
import           Data.Time.Clock                       (getCurrentTime)
import qualified Data.UUID                             as UUID
import           Network.HTTP.Client                   (Request (..),
                                                        RequestBody (..),
                                                        httpNoBody, parseUrl,
                                                        withManager)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           Snap.Core                             hiding (method)
import           System.Random                         (randomIO, randomRIO)
import           System.IO                             (hPutStrLn, stderr)
import           Text.Blaze                            (AttributeValue)
import           Text.Blaze.Html5                      (Html)

import           Application.Star.Ballot
import qualified Application.Star.Ballot               as Ballot
import           Application.Star.BallotStyle
import qualified Application.Star.BallotStyle          as BS
import           Application.Star.HashChain
import           Application.Star.Util                 (MonadAcidState, doQuery,
                                                        doUpdate, errorUpdateShow,
                                                        getBallotStyles, printPDF,
                                                        render)
import           Application.StarTerminal.LinkHelper
import           Application.StarTerminal.Localization
import           Application.StarTerminal.PaperBallot  (paperBallot)
import           Application.StarTerminal.State
import           Application.StarTerminal.View


-- | Accepts ballot codes and records mappings from codes to ballot styles.
-- This function updates the @_ballotCodes@ field of @TerminalState@.
recordBallotStyleCode :: StarTerm m => m ()
recordBallotStyleCode = do
  ballotId  <- param "ballotId"
  code      <- paramR "code"
  ballotStyles <- getBallotStyles
  let style = ballotId >>= flip BS.lookup ballotStyles
  when (isNothing style) $ do
    modifyResponse $ setResponseStatus 404 "Not Found"
    getResponse >>= finishWith
  when (isNothing code) $ do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    getResponse >>= finishWith
  doUpdate $ InsertCode (fromJust code) (fromJust style)

-- | This renders the first page that the voter should see:
-- A prompt asking for a ballot code.
-- When this handler is invoked with a ballot code parameter,
-- looks up the corresponding ballot style and redirects to the first-step page
-- for that ballot style.
askForBallotCode :: StarTerm m => m ()
askForBallotCode = do
  mCode   <- paramR "code"
  mJargon <- getJargonCookie
  case mCode of
    Nothing -> noCode mJargon
    Just c  -> do mStyle <- doQuery (LookupBallotStyle c)
                  case mStyle of
                    Nothing    -> noCode mJargon
                    Just style -> case mJargon of
                      Nothing -> redirect (e url)
                      Just u  -> renderPg (ballotInstructionsView (jargonStrings u) url)
                      where url = firstStepUrl c style
  where noCode     = renderPg . noCodeView
        noCodeView = maybe (codeEntryView strings) (signInView . jargonStrings)

showBallotStep :: StarTerm m => m ()
showBallotStep = do
  (code, ballotStyle, race) <- ballotStepParams
  s <- getSelection ballotStyle race
  renderPg (ballotStepView strings (nav code ballotStyle race) ballotStyle race s)
  where
    nav code style race = NavLinks
      { _prev = ((stepUrl code) . _rId) <$> prevRace style race
      , _next = Just (nextStepUrl code style race)
      , _index = Nothing
      }

recordBallotSelection :: StarTerm m => m ()
recordBallotSelection = do
  (code, style, race) <- ballotStepParams
  s <- getPostParam (e "selection")
  case s of
    Just selection -> do
      setSelection code style race (d selection)
      redirect (e (nextStepUrl code style race))
    Nothing -> pass

showSummary :: StarTerm m => m ()
showSummary = do
  (code, style, ballot) <- ballotParams
  renderPg (summaryView strings code style ballot)

-- | Encrypt the vote, send it to the controller, and redirect to a page with instructions
finalize :: StarTerm m => m ()
finalize = do
  (code, style, ballot) <- ballotParams
  -- TODO: ensure uniqueness of bid and bcid
  ballotId        <- BallotId        . pack . UUID.toString <$> liftIO randomIO
  ballotCastingId <- BallotCastingId . pack . UUID.toString <$> liftIO randomIO
  term            <- doQuery GetTerminalConfig
  record          <- errorUpdateShow $ EncryptRecord
                             (view pubkey term)
                             (view tId term)
                             ballotId
                             ballotCastingId
                             (view zp0 term)
                             (view zi0 term)
                             ballot
  now             <- liftIO getCurrentTime
  ballotContents  <- paperBallot ballot ballotId style record term now
  doUpdate $ RecordVote record
  liftIO $ transmit (view postUrl term) record
  mUseJargon <- getJargonCookie
  case mUseJargon of
    Nothing -> do
      printPDF ballotContents
      redirect (e (exitInstructionsUrl ballotId))
    Just useJargon -> renderPg $
      completionView (jargonStrings useJargon) ballotCastingId

printReceipt :: StarTerm m => m ()
printReceipt = do url <- fmap (ballotReceiptUrl . BallotId . d) <$> getParam "bid"
                  case url of
                    Nothing -> do404
                    Just u -> renderPg (printReceiptView u strings)

-- | Send an encrypted vote to the controller
transmit :: String -> EncryptedRecord -> IO ()
transmit url record = do
  initReq <- parseUrl url
  _ <- withManager mgrSettings $ \mgr -> httpNoBody (request initReq) mgr
  return ()
  where
    body        = RequestBodyLBS (JSON.encode record)
    request r   = r { method = "POST", requestBody = body }
    mgrSettings = tlsManagerSettings

ballotStepParams :: StarTerm m => m (BallotCode, BallotStyle, Race)
ballotStepParams = do
  code   <- paramR "code"
  raceId <- param "stepId"
  case liftA2 (,) code raceId of
    Just (code', raceId') ->
      do style <- doQuery $ LookupBallotStyle code'
         maybe pass return $ do ss <- style
                                r <- bRace raceId' ss
                                return (code', ss, r)
    Nothing -> pass

ballotParams :: StarTerm m => m (BallotCode, BallotStyle, Ballot)
ballotParams = do
  code    <- paramR "code"
  mBallot <- maybe pass getBallot code
  mmStyle <- traverse (doQuery . LookupBallotStyle) code
  maybe pass return (params code (join mmStyle) mBallot)
  where
    params = liftA3 (\x y z -> (x,y,z))

getSelection :: StarTerm m => BallotStyle -> Race -> m (Maybe Selection)
getSelection style race = do
  let k = key style race
  c <- getCookie (e k)
  return $ (d . cookieValue) <$> c

setSelection :: StarTerm m => BallotCode -> BallotStyle -> Race -> Selection -> m ()
setSelection code style race s = modifyResponse $ addResponseCookie (c s)
  where
    k = key style race
    c selection = Cookie
      { cookieName     = e k
      , cookieValue    = e selection
      , cookieExpires  = Nothing
      , cookieDomain   = Nothing
      , cookiePath     = Just (e $ "/ballots/" <> T.pack (show code))
      , cookieSecure   = False  -- TODO: should be True in production
      , cookieHttpOnly = False
      }

getBallot :: StarTerm m => BallotCode -> m (Maybe Ballot)
getBallot code = do
  sty <- doQuery (LookupBallotStyle code)
  case sty of
    Just style -> do
      selections <- mapM (getSel style) (view bRaces style)
      let selections' = catMaybes selections
      let wKeys = map (\(r, s) -> (key style r, s)) selections'
      return $ Just $ foldl' (\ballot (k, s) ->
        Ballot.insert k s ballot)
        Ballot.empty wKeys
    Nothing -> return Nothing
  where
    getSel style race = do
      sel <- getSelection style race
      return $ ((,) race) <$> sel

chooseJargon :: StarTerm m => m Bool
chooseJargon = do
  p <- getParam "content"
  case p of
    Just p' | p' == "jargon" -> return True
            | p' == "gentle" -> return False
    _ -> liftIO randomIO

chooseStylesheet :: StarTerm m => m Text
chooseStylesheet = do
  p <- getParam "presentation"
  case p of
    Just p' -> return (d p')
    Nothing -> (sheets !!) <$> liftIO (randomRIO (0, length sheets-1))
  where
  -- choose spartan less often
  sheets = ["spartan", "solid-select", "solid-select"]

studyWelcome :: StarTerm m => m ()
studyWelcome = do
  useJargon <- chooseJargon
  sheet     <- chooseStylesheet
  modifyResponse $ addResponseCookie (jargonCookie     useJargon)
  modifyResponse $ addResponseCookie (stylesheetCookie sheet    )
  render . page (stylesheet sheet) (localize "star_terminal" strings) $
    welcomeView strings

castBCID, spoilBCID :: StarTerm m => BallotCastingId -> m ()
castBCID bcid = do -- TODO: cast the ballot
  renderPg (castCompletedView strings)
spoilBCID bcid = do -- TODO: spoil the ballot
  Just useJargon <- getJargonCookie
  renderPg (spoilCompletedView (jargonStrings useJargon))

castBallot :: StarTerm m => m ()
castBallot = do
  mCastOrSpoil <- getParam "action"
  mRawBCID     <- getParam "bcid"
  case (mRawBCID, mCastOrSpoil) of
    (Just rawBCID, Just castOrSpoil) -> case reads (Char.unpack rawBCID) of
      (bcid, ""):_
        | castOrSpoil == "cast"  -> castBCID  bcid
        | castOrSpoil == "spoil" -> spoilBCID bcid
      _ -> internalError
    _ -> internalError
  where
  internalError = do
    modifyResponse $ setResponseCode 500
    renderPg internalErrorView

jargonCookie :: Bool -> Cookie
jargonCookie useJargon = Cookie
  { cookieName     = "use-jargon"
  , cookieValue    = if useJargon then "1" else "0"
  , cookieExpires  = Nothing
  , cookieDomain   = Nothing
  , cookiePath     = Just "/"
  , cookieSecure   = False
  , cookieHttpOnly = True
  }

getJargonCookie :: MonadSnap m => m (Maybe Bool)
getJargonCookie = fmap (fmap useJargon) (getCookie "use-jargon")
  where useJargon c = cookieValue c /= "0"

stylesheetCookie :: Text -> Cookie
stylesheetCookie stylesheet = Cookie
  { cookieName     = "stylesheet"
  , cookieValue    = e stylesheet
  , cookieExpires  = Nothing
  , cookieDomain   = Nothing
  , cookiePath     = Just "/"
  , cookieSecure   = False
  , cookieHttpOnly = True
  }

getStylesheetCookie :: MonadSnap m => m AttributeValue
getStylesheetCookie = maybe defaultStylesheet (stylesheet . d . cookieValue)
                   <$> getCookie "stylesheet"

jargonStrings :: Bool -> Translations
jargonStrings useJargon = strings <> if useJargon then jargon else gentle

studyAbout :: StarTerm m => m ()
studyAbout = do
  mJargon <- getJargonCookie
  renderPg (aboutView (jargonStrings (maybe False id mJargon)))

studyStop :: StarTerm m => m ()
studyStop = renderPg (stopView strings)

studyRecordStopReason :: StarTerm m => AcidState Feedback -> m ()
studyRecordStopReason feedbackState = do
  feedback <- getParams
  now      <- liftIO getCurrentTime
  liftIO $ update feedbackState (RecordFeedback (now, feedback))
  redirect "/study/stopped"

feedbackThankYou :: StarTerm m => m ()
feedbackThankYou = renderPg (feedbackView strings)

do404 :: StarTerm m => m ()
do404 = renderPg view404

param :: StarTerm m => Text -> m (Maybe Text)
param k = do
  p <- getParam (encodeUtf8 k)
  return $ fmap (decodeUtf8With ignore) p

paramR :: (StarTerm m, Read a) => Text -> m (Maybe a)
paramR k = do
  p <- param k
  return $ (read . T.unpack) <$> p

e :: Text -> ByteString
e = encodeUtf8

d :: ByteString -> Text
d = decodeUtf8With ignore

renderPage :: StarTerm m => Text -> Html -> m ()
renderPage title html = do
  stylesheet <- getStylesheetCookie
  render (page stylesheet title html)

renderPg :: StarTerm m => Html -> m ()
renderPg = renderPage (localize "star_terminal" strings)

pg :: Html -> Html
pg = page defaultStylesheet (localize "star_terminal" strings)

-- | Adhoc i18n system, for use until a real i18n library is incorporated.
strings, jargon, gentle :: Translations
strings = translations
  [ ("ballot_code_label", "Ballot code:")
  , ("collect_ballot_and_receipt", "Your completed ballot and receipt are printing now. To cast your vote, deposit your ballot into a ballot box. Keep the receipt - you can use it later to make sure that your vote was counted.")
  , ("enter_ballot_code", "Enter a ballot code to begin voting")
  , ("next_step", "next step")
  , ("previous_step", "previous step")
  , ("print_ballot", "done with this ballot")
  , ("select_candidate", "Please select a candidate")
  , ("show_progress", "show progress")
  , ("star_terminal", "STAR Terminal")
  , ("submit", "Submit")
  , ("successful_vote", "You voted!")
  , ("summary", "Review your selections")
  , ("summary_instructions", "Click any race title to change your selection for that race.")
  , ("welcome_to_study", "Welcome to the study")
  , ("study_description", "Thank you for agreeing to participate in a study of a prototype voting system. This is an early prototype, so some parts are still rough. But your participation will help us make it better.\nThe study will include having you try voting using our prototype. Then we will ask you some questions afterwards. The study will take about fifteen minutes in total.\nBefore we begin, make sure you that have the “voter access code” that was included in the email.\nYou are free to stop your participation at any time.")
  , ("ready_to_begin", "I am ready to begin the study")
  , ("not_ready_to_begin", "No thanks, I do not want to participate")
  , ("about_the_prototype", "About the prototype")
  , ("stop", "Stop")
  , ("proceed", "Proceed")
  , ("sign_in", "Sign in")
  , ("enter_email_code", "Enter in the voter access code that was given to you in email.")
  , ("sign_in_button", "Sign in")
  , ("filling_out_your_ballot", "Filling out your ballot")
  , ("mock_election", "For this study, we have a mock election with three races and several candidates for each race.")
  , ("select_candidate", "Select a candidate and press \"next step\".")
  , ("ballot_complete", "Ballot complete")
  , ("congratulations", "Congratulations, you have completed this ballot.")
  , ("thank_you", "Thank you for casting your ballot.")
  , ("answer_questions", "We hope you will take a few minutes to discuss your experience with our moderator.")
  , ("you_voted", "Ballot cast. You voted!")
  , ("exit_study", "exit the study")
  , ("another_ballot", "fill out another ballot")
  , ("thank_you_for_participation", "Thank you for your participation")
  , ("why_stop", "Please take a minute to tell us why you are choosing to stop at this point:")
  , ("reasons_for_stopping", "Too busy\nStudy does not look interesting\nI am already confused at this point")
  , ("other_reason_for_stopping", "Other:")
  , ("feedback_is_nice", "Your feedback is appreciated!")
  , ("portrait", "portrait")
  , ("no_portrait", "no portrait available")
  ]

jargon = strings <> translations
  [ ("special_feature_intro", "This voting system has a special feature called VERIFIABLE VOTING. With verifiable voting, you can:")
  , ("special_feature_bullets", "\"Spoil\" your ballot to challenge the voting terminal to prove that it is behaving correctly.\nVerify with a voting receipt that your ballot is included in the electronic ballot box.\nCheck that the election tally was computed from all the ballots in the electronic ballot box.")
  , ("special_feature_outro", "You do not have to verify your ballot if you do not want to.")
  , ("verify_your_vote", "Remember, you can verify your vote if you wish.")
  , ("cast_or_spoil", "Next, you can either cast this ballot or invalidate/spoil it. Invalidated (spoiled) ballots can be used to check that the election system is working properly.")
  , ("cast", "Cast")
  , ("spoil", "Spoil")
  , ("spoiled_ballot", "Ballot invalidated")
  , ("spoiled_explanation", "Your ballot has been spoiled. After the election, your ballot will be decrypted and posted to a public bulletin board, and you may verify that it contains the vote you recorded.")
  ]

gentle = strings <> translations
  [ ("special_feature_intro", "This voting system prototype has a special feature called PRACTICE BALLOTS. With practice ballots, you can:")
  , ("special_feature_bullets", "See the list of races and candidates and how they appear on the ballot, letting you pause to do some research on who you want to vote for.\nTry out the candidate selection process to make sure you can do it without making mistakes.\nReveal your ballot to make sure it was created properly and was not tampered with by some part of a potentially hacked voting system.")
  , ("special_feature_outro", "You can do as many practice ballots as you want. In fact, we encourage you to do several practice ballots. Eventually, we want you to cast your final ballot for this study. You do not need to tell us ahead of time when you are practicing and when you are doing your \"real\" ballot.")
  , ("verify_your_vote", "Remember, you can do as many practice ballots as you wish.")
  , ("cast_or_spoil", "Next, you can either cast this ballot, or treat it like a practice ballot. Practice ballots let you check that the election system is working properly.")
  , ("cast", "Cast")
  , ("spoil", "Practice")
  , ("spoiled_ballot", "Practice ballot")
  , ("spoiled_explanation", "This ballot will not be counted.")
  ]
