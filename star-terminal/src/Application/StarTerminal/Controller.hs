{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.Controller where

import           Data.CaseInsensitive (mk)
import           Data.Monoid ((<>))
import           Data.Text (Text, unpack)
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
  let ballot  = ballotId >>= flip lookup ballotStyles
  let race = lookupRace ballot stepId
  maybe pass (\r ->
    render (p (navbar strings <> ballotStepView strings r)))
    race
  where
    p = page (localize "star_terminal" strings)
    lookupRace ballot index = do
      b <- ballot
      i <- index >>= parseInt
      bRace i b

ballotHandler :: Snap ()
ballotHandler = do
  modifyRequest $
    rqSetParam (encodeUtf8 "stepId") [encodeUtf8 "0"]
  ballotStepHandler

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
    ])
  ]

