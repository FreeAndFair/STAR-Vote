{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

{- # OPTIONS_GHC -fdefer-type-errors #-}
module Application.StarTerminal.PaperBallot (paperBallot) where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens                    (makeLenses, view)
import           Control.Monad                   (liftM, void)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.CryptoRandom

import           Data.Acid
import           Data.Acid.Advanced
import qualified Data.Binary                     as Binary
import qualified Data.ByteString.Base16.Lazy     as Base16
import qualified Data.ByteString.Base64.Lazy     as Base64
import           Data.ByteString.Lazy            (ByteString, toStrict)
import qualified Data.ByteString.Lazy.Char8      as BS
import           Data.Foldable                   (find, foldrM)
import qualified Data.Map                        as M
import           Data.Monoid
import           Data.SafeCopy
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error        (ignore)
import           Data.Time                       (UTCTime, formatTime)
import           Data.Typeable

import           Graphics.Barcode.Code128
import           Graphics.PDF

import           System.Locale                   (defaultTimeLocale)


import           Application.Star.Ballot
import           Application.Star.BallotStyle
import           Application.Star.HashChain
import           Application.Star.Instances ()
import           Application.Star.SerializableBS (fromSB)
import           Application.Star.Util

import           Application.StarTerminal.State

import           Debug.Trace

receiptFont :: PDFFont
receiptFont = PDFFont Helvetica 16

receiptStyle, ballotIdStyle :: StandardStyle
receiptStyle = Font receiptFont black black

ballotIdStyle = Font (PDFFont Helvetica 10) black black

ballotBarcodeConfig :: BarcodeConfig
ballotBarcodeConfig = BarcodeConfig { height = 50, barWidth = 1.0 }

ballotWidth, ballotHeight :: Int
ballotWidth = 500
ballotHeight = 700


receiptPageContents :: TerminalId -> UTCTime -> String -> Draw ()
receiptPageContents term t hashView = do
  displayFormattedText (Rectangle (10 :+ 10) (690 :+ 690))  NormalParagraph receiptStyle $ do
    paragraph . txt $ "Terminal ID: " ++ (T.unpack . decodeUtf8With ignore . toStrict . fromSB) termId
    paragraph . txt $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t
    paragraph . txt $ "Confirmation code: " ++ hashView
  where (TerminalId termId) = term


-- | All of the details about a race selection that are necessary for
-- the printed ballot
data ElaboratedSelection = ElaboratedSelection
  { _esRace   :: Race
  , _esOption :: Option
  } deriving Typeable
$(makeLenses ''ElaboratedSelection)

-- | All the data necessary about a ballot to print it on paper
data ElaboratedBallot = ElaboratedBallot
  { _ebId          :: BallotId
  , _ebBcid        :: BallotCastingId
  , _ebCastingTime :: UTCTime
  , _ebPublicHash  :: PublicHash
  , _ebSelections  :: [ElaboratedSelection]
  } deriving Typeable
$(makeLenses ''ElaboratedBallot)


-- | Construct an "elaborated ballot". In other words, we gather
-- together all of the information that will be displayed on the paper
-- ballot.
elabBallot :: Ballot -> BallotId -> BallotStyle -> EncryptedRecord -> UTCTime -> ElaboratedBallot
elabBallot ballot bid bs enc t =
  ElaboratedBallot bid (view bcid enc) t (view zp enc)
    [ ElaboratedSelection r opt
    | rs@(RaceSelection (k, sel)) <- races ballot
    , let Just (_, rid) = fromKey k
    , let Just r = bRace rid bs
    , let Just opt = option sel r
    ]

-- | Draw barcodes and text for a single ballot selection - this is a
-- component of c_bid. Returns the lower-right coordinates.
drawSelection :: ElaboratedSelection -> Point -> Draw Point
drawSelection (ElaboratedSelection race opt) start@(x :+ y) = do
    displayFormattedText
        (Rectangle (start - (0 :+ textHeight)) (start + (400 :+ 0)))
        NormalParagraph ballotIdStyle $ do
     paragraph . txt $ "Race: " ++ T.unpack (view rDescription race)

    let start' = start - (0 :+ textHeight)
    corner <- fromRight $
                drawBarcode barcodeConfig (T.unpack $ view oId opt)
                  (start' - (0 :+ textHeight))
    displayFormattedText
        (Rectangle (corner - (0 :+ textHeight)) (corner + (400 :+ 0)))
        NormalParagraph ballotIdStyle $ do
      paragraph . txt $ "Vote: " ++ T.unpack (view oName opt) ++ maybe "" ((++")") . (" ("++) . T.unpack) (view oParty opt)
    return $ start + (0 :+ (0 - 1.2*textHeight - height barcodeConfig))
  where barcodeConfig = BarcodeConfig { height = 20, barWidth = 1.0 }
        textHeight = 20
        fromRight (Right x) = x


-- | Draw the ballot ID at the top of the page
drawBallotId :: BallotId -> BallotCastingId -> Draw ()
drawBallotId (BallotId bid) (BallotCastingId bcid) = do
  let barcodeStartCorner =
        (30 :+ (fromIntegral ballotHeight - 30 - (height ballotBarcodeConfig)))
  (_ :+ nextStart) <- drawId "Casting ID: " bcid barcodeStartCorner
  drawId "Paper ID: " bid (30 :+ (nextStart - height ballotBarcodeConfig))
  return ()

  where
    drawId label id startCorner = do
      (_ :+ y) <-
        case drawBarcode ballotBarcodeConfig (T.unpack id) startCorner of
          Left err -> fail (show err)
          Right draw -> draw
      let idTextBottomLeft = (35 :+ (y - height ballotBarcodeConfig - 15))
          idTextTopRight = ((fromIntegral ballotWidth - 30) :+ (y - height ballotBarcodeConfig - 5))
          idTextRect = Rectangle idTextBottomLeft idTextTopRight
      displayFormattedText idTextRect NormalParagraph ballotIdStyle $ do
        paragraph . txt $ label <> T.unpack id
      return idTextBottomLeft

-- | Generate a paper ballot and voter reciept as a PDF
paperBallot :: StarTerm m => Ballot -> BallotId -> BallotStyle -> EncryptedRecord -> Terminal -> UTCTime -> m ByteString
paperBallot ballot bid style enc term t = do
  let elaboratedBallot = elabBallot ballot bid style enc t
      rect = PDFRect 0 0 ballotWidth ballotHeight
  liftIO $ pdfByteString standardDocInfo { compressed = False } rect $ do
    ballotPage <- addPage Nothing
    void . drawWithPage ballotPage $ do
      drawBallotId (view ebId elaboratedBallot) (view ebBcid elaboratedBallot)
      foldrM drawSelection (20 :+ 500 ) (view ebSelections elaboratedBallot)
    receiptPage <- addPage Nothing
    drawWithPage receiptPage (receiptPageContents (view tId term) t hashView)
  where (PublicHash hash) = view zp enc
        hashView = take 20 . T.unpack . decodeUtf8With ignore . toStrict . Base16.encode . fromSB $ hash

