{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.StarTerminal.Controller where

import           Control.Applicative ((<$>))
import           Control.Monad.Except (MonadError)
import           Control.Monad.State (MonadState)
import           Data.ByteString (ByteString)
import           Data.CaseInsensitive (mk)
import           Data.List (foldl')
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (ignore)
import           Snap.Core
import           Text.Blaze.Html5 (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import           Application.Star.Ballot
import qualified Application.Star.Ballot as Ballot
import           Application.Star.BallotStyle
import qualified Application.Star.BallotStyle as BS
import           Application.StarTerminal.LinkHelper
import           Application.StarTerminal.Localization
import           Application.StarTerminal.View
import           Application.StarTerminal.State

type StarTerm m = (MonadError Text m, MonadState TerminalState m, MonadSnap m)

ballotHandler :: StarTerm m => m ()
ballotHandler = do
  ballotId <- param "ballotId"
  let style = ballotId >>= flip BS.lookup ballotStyles
  maybe pass (\s -> redirect (e (firstStepUrl s))) style

showBallotStep :: StarTerm m => m ()
showBallotStep = do
  (ballotStyle, race) <- ballotStepParams
  s <- getSelection ballotStyle race
  render (p (ballotStepView strings (nav ballotStyle race) race s))
  where
    nav style race = NavLinks
      { _prev = ((stepUrl (_bId style)) . _rId) <$> prevRace style race
      , _next = Just (nextStepUrl style race)
      , _index = Nothing
      }

recordBallotSelection :: StarTerm m => m ()
recordBallotSelection = do
  (style, race) <- ballotStepParams
  s <- getPostParam (e "selection")
  case s of
    Just selection -> do
      setSelection style race (d selection)
      redirect (e (nextStepUrl style race))
    Nothing -> pass

showSummary :: StarTerm m => m ()
showSummary = do
  (style, ballot) <- ballotParams
  render (p (summaryView strings style ballot))

finalize :: StarTerm m => m ()
finalize = do
  (style, ballot) <- ballotParams
  -- transmit ballot
  redirect (e (exitInstructionsUrl style))

exitInstructions :: StarTerm m => m ()
exitInstructions = render (p (exitInstructionsView strings))

transmit :: Ballot -> IO ()
transmit = undefined -- TODO

ballotStepParams :: StarTerm m => m (BallotStyle, Race)
ballotStepParams = do
  ballotId <- param "ballotId"
  raceId   <- param "stepId"
  maybe pass return (params ballotId raceId)
  where
    params mBId mRId = do
      bId    <- mBId
      rId    <- mRId
      style  <- BS.lookup bId ballotStyles
      race   <- bRace rId style
      return (style, race)

ballotParams :: StarTerm m => m (BallotStyle, Ballot)
ballotParams = do
  mBId    <- param "ballotId"
  mBallot <- maybe pass getBallot mBId
  maybe pass return (params mBId mBallot)
  where
    params mBId mBallot = do
      bId    <- mBId
      style  <- BS.lookup bId ballotStyles
      ballot <- mBallot
      return (style, ballot)

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

getBallot :: StarTerm m => BallotStyleId -> m (Maybe Ballot)
getBallot bId = do
  case BS.lookup bId ballotStyles of
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

e :: Text -> ByteString
e = encodeUtf8

d :: ByteString -> Text
d = decodeUtf8With ignore

p :: Html -> Html
p = page (localize "star_terminal" strings)

strings :: Translations
strings = translations
  [ ("collect_ballot_and_receipt", "Your completed ballot and receipt are printing now. To cast your vote, deposit your ballot into a ballot box. Keep the receipt - you can use it later to make sure that your vote was counted.")
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
