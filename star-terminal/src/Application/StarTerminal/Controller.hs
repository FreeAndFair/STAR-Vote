{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.Controller where

import           Data.CaseInsensitive (mk)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (ignore)
import           Numeric (readDec)
import           Snap.Core
import           Text.Blaze.Html5 (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import           Application.StarTerminal.BallotStyle
import           Application.StarTerminal.Localization
import           Application.StarTerminal.View

ballotStepHandler :: Snap ()
ballotStepHandler = do
  modifyResponse $ setContentType "text/html"
                 . setHeader (mk "Cache-Control") "max-age=0"
  ballotId <- param "ballotId"
  stepId   <- param "stepId"
  maybe pass (\(bId, idx, race) ->
    render (p (ballotStepView strings (nav bId idx) race)))
    (params ballotId stepId)
  where
    p = page (localize "star_terminal" strings)
    params mBId mIdx = do
      bId    <- mBId
      idx    <- mIdx >>= parseInt
      ballot <- lookup bId ballotStyles
      race   <- bRace idx ballot
      return (bId, idx, race)
    nav bId idx = NavLinks
      { _prev = if idx > 0 then Just (stepUrl bId (idx - 1)) else Nothing
      , _next = if idx < max then Just (stepUrl bId (idx + 1)) else Nothing
      , _index = Nothing
      }
    max = 999 -- TODO

ballotHandler :: Snap ()
ballotHandler = do
  modifyRequest $
    rqSetParam (encodeUtf8 "stepId") [encodeUtf8 "0"]
  ballotStepHandler

stepUrl :: BallotId -> Int -> Text
stepUrl bId idx = T.concat ["/ballot/", bId, "/step/", pack (show idx)]

render :: Html -> Snap ()
render = writeLBS . renderHtml

param :: MonadSnap m => Text -> m (Maybe Text)
param k = do
  p <- getParam (encodeUtf8 k)
  return $ fmap (decodeUtf8With ignore) p

parseInt :: Text -> Maybe Int
parseInt = safeHead . map fst . readDec . unpack
  where
    safeHead (x:_) = Just x
    safeHead []     = Nothing

strings :: Translations
strings = translations
  [ ("next_step", "next step")
  , ("previous_step", "previous step")
  , ("select_candidate", "Please select a candidate")
  , ("show_progress", "show progress")
  , ("star_terminal", "STAR Terminal")
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

