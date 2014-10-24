{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Application.StarTerminal.Controller
Description : Defines web server actions

Defines handlers, which are called from `Main`.
Handles state changes,
invokes functions in `Application.StarTerminal.View` to render pages.
 -}
module Application.StarTerminal.Controller where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (when)
import           Control.Monad.Except (MonadError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (MonadState, get, state)
import qualified Data.Aeson as JSON
import           Data.ByteString (ByteString)
import           Data.CaseInsensitive (mk)
import           Data.List (foldl')
import           Data.Maybe (catMaybes, fromJust, isNothing)
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (ignore)
import qualified Data.UUID as UUID
import           Network.HTTP.Client ( Request(..)
                                     , RequestBody(..)
                                     , httpNoBody
                                     , parseUrl
                                     , withManager )
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Snap.Core hiding (method)
import           System.Random (randomIO)
import           Text.Blaze.Html5 (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import           Application.Star.Ballot
import qualified Application.Star.Ballot as Ballot
import           Application.Star.BallotStyle
import qualified Application.Star.BallotStyle as BS
import           Application.Star.HashChain
import           Application.StarTerminal.LinkHelper
import           Application.StarTerminal.Localization
import           Application.StarTerminal.View
import           Application.StarTerminal.State

type StarTerm m = (MonadError Text m, MonadState TerminalState m, MonadSnap m)

-- | Accepts ballot codes and records mappings from codes to ballot styles.
-- This function updates the `_ballotCodes` field of `TerminalState`.
recordBallotStyleCode :: StarTerm m => m ()
recordBallotStyleCode = do
  ballotId  <- param "ballotId"
  code      <- paramR "code"
  let style = ballotId >>= flip BS.lookup ballotStyles
  when (isNothing style) $ do
    modifyResponse $ setResponseStatus 404 "Not Found"
    getResponse >>= finishWith
  when (isNothing code) $ do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    getResponse >>= finishWith
  state $ ((,) ()) . insertCode (fromJust code) (fromJust style)

-- | This renders the first page that the voter should see:
-- A prompt asking for a ballot code.
-- When this handler is invoked with a ballot code parameter,
-- looks up the corresponding ballot style and redirects to the first-step page
-- for that ballot style.
askForBallotCode :: StarTerm m => m ()
askForBallotCode = do
  mCode  <- paramR "code"
  tState <- get
  let mStyle = mCode >>= flip lookupBallotStyle tState
  case (,) <$> mCode <*> mStyle of
    Just (code, style) -> redirect (e (firstStepUrl code style))
    Nothing            -> render (pg (codeEntryView strings))

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
      setSelection style race (d selection)
      redirect (e (nextStepUrl code style race))
    Nothing -> pass

showSummary :: StarTerm m => m ()
showSummary = do
  (code, style, ballot) <- ballotParams
  render (pg (summaryView strings code style ballot))

finalize :: StarTerm m => m ()
finalize = do
  (code, _, ballot) <- ballotParams
  ballotId        <- BallotId        . pack . UUID.toString <$> liftIO randomIO
  ballotCastingId <- BallotCastingId . pack . UUID.toString <$> liftIO randomIO
  tState          <- get
  let term   = _terminal tState
  let record = encryptRecord (_pubkey term)
                             (_tId term)
                             ballotId
                             ballotCastingId
                             (_zp0 term)
                             (_zi0 term)
                             ballot
  state $ \s -> ((), s { _recordedVotes = record : _recordedVotes s })
  liftIO $ transmit (_postUrl term) record
  redirect (e (exitInstructionsUrl code))

exitInstructions :: StarTerm m => m ()
exitInstructions = render (pg (exitInstructionsView strings))

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
  tState <- get
  maybe pass return (params code raceId tState)
  where
    params mCode mRId s = do
      code   <- mCode
      rId    <- mRId
      style  <- lookupBallotStyle code s
      race   <- bRace rId style
      return (code, style, race)

ballotParams :: StarTerm m => m (BallotCode, BallotStyle, Ballot)
ballotParams = do
  code    <- paramR "code"
  mBallot <- maybe pass getBallot code
  tState  <- get
  maybe pass return (params code mBallot tState)
  where
    params mCode mBallot s = do
      code   <- mCode
      style  <- lookupBallotStyle code s
      ballot <- mBallot
      return (code, style, ballot)

getSelection :: StarTerm m => BallotStyle -> Race -> m (Maybe Selection)
getSelection style race = do
  let k = key style race
  c <- getCookie (e k)
  return $ (d . cookieValue) <$> c

setSelection :: StarTerm m => BallotStyle -> Race -> Selection -> m ()
setSelection style race s = modifyResponse $ addResponseCookie (c s)
  where
    k = key style race
    c selection = Cookie
      { cookieName     = e k
      , cookieValue    = e selection
      , cookieExpires  = Nothing
      , cookieDomain   = Nothing
      , cookiePath     = Just (e "/")
      , cookieSecure   = False  -- TODO: should be True in production
      , cookieHttpOnly = True
      }

getBallot :: StarTerm m => BallotCode -> m (Maybe Ballot)
getBallot code = do
  tState <- get
  case lookupBallotStyle code tState of
    Just style -> do
      selections <- mapM (getSel style) (bRaces style)
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

render :: StarTerm m => Html -> m ()
render h = do
  modifyResponse $ setContentType "text/html"
                 . setHeader (mk "Cache-Control") "max-age=0"
  writeLBS (renderHtml h)

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

strings :: Translations
strings = translations
  [ ("ballot_code_label", "Ballot code:")
  , ("collect_ballot_and_receipt", "Your completed ballot and receipt are printing now. To cast your vote, deposit your ballot into a ballot box. Keep the receipt - you can use it later to make sure that your vote was counted.")
  , ("enter_ballot_code", "Enter a ballot code to begin voting")
  , ("next_step", "next step")
  , ("previous_step", "previous step")
  , ("print_ballot", "print ballot to proceed")
  , ("select_candidate", "Please select a candidate")
  , ("show_progress", "show progress")
  , ("star_terminal", "STAR Terminal")
  , ("submit", "Submit")
  , ("successful_vote", "You voted!")
  , ("summary", "Review and finalize your selections")
  ]

ballotStyles :: BallotStyles
ballotStyles =
  [ BallotStyle
    { _bId = "oregon-2014"
    , _bRaces =
      [ Race
        { _rDescription = "Oregon Governor"
        , _rId = "gov"
        , _rOptions =
          [ Option "c1" "Aaron Auer"        (Just "Con") (Just "Minister of the Gospel")
          , Option "c2" "Tovia E Fornah"    (Just "Non") (Just "Service")
          , Option "c3" "Paul Grad"         (Just "L")   (Just "Investor")
          , Option "c4" "Chris Henry"       (Just "P")   Nothing
          , Option "c5" "John Kitzhaber"    (Just "Dem") (Just "Governor of Oregon")
          , Option "c6" "Jason Levin"       (Just "Grn") (Just "Cannabis Industry Professional")
          , Option "c7" "Dennis Richardson" (Just "Rep") (Just "Businessman; State Representative")
          ]
        }
      , Race
        { _rDescription = "US Senator"
        , _rId = "senate"
        , _rOptions =
          [ Option "s1" "James E. Leuenberger" (Just "Con") Nothing
          , Option "s2" "Christina Jean Lugo"  (Just "Grn") (Just "Artist, Peace Activist")
          , Option "s3" "Jeff Merkley"         (Just "Dem") (Just "United States Senator")
          , Option "s4" "Mike Montchalin"      (Just "L")   (Just "Candidate/Retired")
          , Option "s5" "Monica Wehby"         (Just "Rep") (Just "Pediatric Neurosurgeon")
          ]
        }
      , Race
        { _rDescription = "US Representative, 3rd District"
        , _rId = "rep_3"
        , _rOptions =
          [ Option "r1" "Earl Blumenauer"  (Just "Dem") (Just "U.S. Congressman")
          , Option "r2" "James Buchal"     (Just "Rep") (Just "Attorney")
          , Option "r3" "Jeffrey J Langan" (Just "L")   Nothing
          , Option "r4" "Michael Meo"      (Just "Grn") (Just "retired schoolteacher")
          , Option "r5" "David Walker"     (Just "Non") (Just "Family Nurse Practitioner")
          ]
        }
      ]
    }
  ]
