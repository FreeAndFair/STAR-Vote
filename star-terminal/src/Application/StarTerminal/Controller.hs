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
Upon selecting \"print ballot to proceed\", a POST request is made,
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
import qualified Data.Aeson                            as JSON
import           Data.ByteString                       (ByteString)
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
import           System.Random                         (randomIO)
import           Text.Blaze.Html5                      (Html)

import           Application.Star.Ballot
import qualified Application.Star.Ballot               as Ballot
import           Application.Star.BallotStyle
import qualified Application.Star.BallotStyle          as BS
import           Application.Star.HashChain
import           Application.Star.Util                 (MonadAcidState, doQuery,
                                                        doUpdate, errorUpdateShow,
                                                        getBallotStyles, render)
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
  mCode  <- paramR "code"
  case mCode of
    Just c -> do mStyle <- doQuery (LookupBallotStyle c)
                 case liftA2 (,) mCode mStyle of
                   Just (code, style) -> redirect (e (firstStepUrl code style))
                   Nothing            -> noCode
    Nothing -> noCode
  where noCode = render (pg (codeEntryView strings))

showBallotStep :: StarTerm m => m ()
showBallotStep = do
  (code, ballotStyle, race) <- ballotStepParams
  s <- getSelection ballotStyle race
  render (pg (ballotStepView strings (nav code ballotStyle race) ballotStyle race s))
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
  render (pg (summaryView strings code style ballot))

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
  now <- liftIO getCurrentTime
  doUpdate $ RecordVote record
  doUpdate $ SaveReceipt ballotId ballot style record now
  liftIO $ transmit (view postUrl term) record
  redirect (e (exitInstructionsUrl ballotId))

-- TODO: the terminal needs to print the human-readable ballot and receipt
printReceipt :: StarTerm m => m ()
printReceipt = do url <- fmap (ballotReceiptUrl . BallotId . d) <$> getParam "bid"
                  case url of
                    Nothing -> do404
                    Just u -> render (pg (printReceiptView u strings))


printReceiptPDF :: StarTerm m => m ()
printReceiptPDF = do
  tm <- doQuery GetTerminalConfig
  bid' <- fmap (BallotId . decodeUtf8With handler) <$> getParam "bid"
  case bid' of
    Nothing -> noBallot
    Just bid -> do
      foundInfo <- doQuery $ GetReceipt bid
      case foundInfo of
        Nothing -> noBallot
        Just (ballot, ballotStyle, encrypted, voteTime) -> do
          ballotContents <- paperBallot ballot bid ballotStyle encrypted tm voteTime
          modifyResponse $ setContentType "application/pdf"
          writeLBS ballotContents


  where noBallot = do modifyResponse $ setResponseStatus 404 "Not Found"
                      getResponse >>= finishWith
        handler msg input = error $ "Failed to decode UTF8: " ++ show input

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


do404 :: StarTerm m => m ()
do404 = render (pg view404)

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

pg :: Html -> Html
pg = page (localize "star_terminal" strings)

-- | Adhoc i18n system, for use until a real i18n library is incorporated.
strings :: Translations
strings = translations
  [ ("ballot_code_label", "Ballot code:")
  , ("collect_ballot_and_receipt", "Your completed ballot and receipt are printing now. To cast your vote, deposit your ballot into a ballot box. Keep the receipt - you can use it later to make sure that your vote was counted.")
  , ("enter_ballot_code", "Enter a ballot code to begin voting")
  , ("next_step", "next step")
  , ("previous_step", "previous step")
  , ("print_ballot", "print ballot to proceed")
  , ("print_ballot_receipt_now", "Print your ballot and receipt now")
  , ("select_candidate", "Please select a candidate")
  , ("show_progress", "show progress")
  , ("star_terminal", "STAR Terminal")
  , ("submit", "Submit")
  , ("successful_vote", "You voted!")
  , ("summary", "Review and finalize your selections")
  ]
