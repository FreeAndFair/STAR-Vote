{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.Controller where

import           Control.Applicative ((<$>))
import           Data.ByteString (ByteString)
import           Data.CaseInsensitive (mk)
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (ignore)
import           Snap.Core
import           Text.Blaze.Html5 (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import           Application.StarTerminal.Ballot
import           Application.StarTerminal.BallotStyle
import           Application.StarTerminal.Localization
import           Application.StarTerminal.View

ballotHandler :: Snap ()
ballotHandler = do
  ballotId <- param "ballotId"
  maybe pass (\bId ->
    redirect (e (stepUrl bId 0)))
    ballotId

showBallotStep :: Snap ()
showBallotStep = do
  (bId, idx, race) <- ballotStepParams
  modifyResponse $ setContentType "text/html"
                 . setHeader (mk "Cache-Control") "max-age=0"
  s <- getSelection bId idx
  render (p (ballotStepView strings (nav bId idx) race s))
  where
    p = page (localize "star_terminal" strings)
    nav bId idx = NavLinks
      { _prev = if idx > 0 then Just (stepUrl bId (idx - 1)) else Nothing
      , _next = if idx < max then Just (stepUrl bId (idx + 1)) else Nothing
      , _index = Nothing
      }
    max = 999 -- TODO

recordBallotSelection :: Snap ()
recordBallotSelection = do
  (bId, idx, _) <- ballotStepParams
  s <- getPostParam (e "selection")
  case s of
    Just selection -> do
      setSelection bId idx (d selection)
      redirect (e (stepUrl bId (idx + 1)))
    Nothing -> pass

ballotStepParams :: Snap (BallotId, Int, Race)
ballotStepParams = do
  ballotId <- param "ballotId"
  stepId   <- param "stepId"
  maybe pass return (params ballotId stepId)
  where
    params mBId mIdx = do
      bId    <- mBId
      idx    <- mIdx >>= parseInt
      ballot <- Prelude.lookup bId ballotStyles
      race   <- bRace idx ballot
      return (bId, idx, race)

getSelection :: MonadSnap m => BallotId -> Int -> m (Maybe Selection)
getSelection bId idx = do
  let k = key bId idx
  c <- getCookie (e k)
  return $ (d . cookieValue) <$> c

setSelection :: MonadSnap m => BallotId -> Int -> Selection -> m ()
setSelection bId idx s = modifyResponse $ addResponseCookie (c s)
  where
    k = key bId idx
    c selection = Cookie
      { cookieName     = e k
      , cookieValue    = e selection
      , cookieExpires  = Nothing
      , cookieDomain   = Nothing
      , cookiePath     = Just (e "/")
      , cookieSecure   = False  -- TODO: should be True in production
      , cookieHttpOnly = True
      }

stepUrl :: BallotId -> Int -> Text
stepUrl bId idx = T.concat ["/ballot/", bId, "/step/", pack (show idx)]

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
  ]

ballotStyles :: BallotStyles
ballotStyles =
  [ ("oregon-2014", BallotStyle
    [ Race
      { _rDescription = "Oregon Governor"
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
      , _rOptions =
        [ Option "r1" "Earl Blumenauer"  (Just "Dem") (Just "U.S. Congressman")
        , Option "r2" "James Buchal"     (Just "Rep") (Just "Attorney")
        , Option "r3" "Jeffrey J Langan" (Just "L")   Nothing
        , Option "r4" "Michael Meo"      (Just "Grn") (Just "retired schoolteacher")
        , Option "r5" "David Walker"     (Just "Non") (Just "Family Nurse Practitioner")
        ]
      }
    ])
  ]

