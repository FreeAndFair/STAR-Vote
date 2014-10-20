{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.Controller where

import           Control.Applicative ((<$>))
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

import           Application.StarTerminal.Ballot
import qualified Application.StarTerminal.Ballot as Ballot
import           Application.StarTerminal.BallotStyle
import qualified Application.StarTerminal.BallotStyle as BS
import           Application.StarTerminal.LinkHelper
import           Application.StarTerminal.Localization
import           Application.StarTerminal.View

ballotHandler :: Snap ()
ballotHandler = do
  ballotId <- param "ballotId"
  let style = ballotId >>= flip BS.lookup ballotStyles
  maybe pass (\s -> redirect (e (firstStepUrl s))) style

showBallotStep :: Snap ()
showBallotStep = do
  (ballotStyle, race) <- ballotStepParams
  modifyResponse $ setContentType "text/html"
                 . setHeader (mk "Cache-Control") "max-age=0"
  s <- getSelection ballotStyle race
  render (p (ballotStepView strings (nav ballotStyle race) race s))
  where
    p = page (localize "star_terminal" strings)
    nav style race = NavLinks
      { _prev = ((stepUrl (_bId style)) . _rId) <$> prevRace style race
      , _next = Just (nextStepUrl style race)
      , _index = Nothing
      }

recordBallotSelection :: Snap ()
recordBallotSelection = do
  (style, race) <- ballotStepParams
  s <- getPostParam (e "selection")
  case s of
    Just selection -> do
      setSelection style race (d selection)
      redirect (e (nextStepUrl style race))
    Nothing -> pass

showSummary :: Snap ()
showSummary = do
  mBId    <- param "ballotId"
  mBallot <- maybe pass getBallot mBId
  case params mBId mBallot of
    Just (style, ballot) ->
      render (p (summaryView strings style ballot))
    Nothing -> pass
  where
    params mBId mBallot = do
      bId    <- mBId
      style  <- BS.lookup bId ballotStyles
      ballot <- mBallot
      return (style, ballot)
    p = page (localize "star_terminal" strings)

ballotStepParams :: Snap (BallotStyle, Race)
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

getSelection :: MonadSnap m => BallotStyle -> Race -> m (Maybe Selection)
getSelection style race = do
  let k = key style race
  c <- getCookie (e k)
  return $ (d . cookieValue) <$> c

setSelection :: MonadSnap m => BallotStyle -> Race -> Selection -> m ()
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

getBallot :: MonadSnap m => BallotId -> m (Maybe Ballot)
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

render :: Html -> Snap ()
render = writeLBS . renderHtml

param :: MonadSnap m => Text -> m (Maybe Text)
param k = do
  p <- getParam (encodeUtf8 k)
  return $ fmap (decodeUtf8With ignore) p

e :: Text -> ByteString
e = encodeUtf8

d :: ByteString -> Text
d = decodeUtf8With ignore

strings :: Translations
strings = translations
  [ ("next_step", "next step")
  , ("previous_step", "previous step")
  , ("select_candidate", "Please select a candidate")
  , ("show_progress", "show progress")
  , ("star_terminal", "STAR Terminal")
  , ("submit", "Submit")
  , ("summary", "Review and finalize your selections")
  ]

ballotStyles :: BallotStyles
ballotStyles =
  [ BallotStyle
    { _bId = "oregon-2014"
    , _bRaces = [ Race
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
