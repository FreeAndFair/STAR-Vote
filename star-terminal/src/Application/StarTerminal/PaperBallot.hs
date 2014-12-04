{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- # OPTIONS_GHC -fdefer-type-errors #-}
module Application.StarTerminal.PaperBallot (paperBallot) where

import Control.Applicative
import           Control.Arrow
import           Control.Lens                    (makeLenses, view)
import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO, liftIO)

import qualified Data.Binary as Binary
import           Data.ByteString.Lazy            (ByteString, toStrict)
import qualified Data.ByteString.Base16.Lazy     as Base16
import           Data.Foldable                   (find, foldrM)
import qualified Data.Map                        as M
import           Data.Monoid
import qualified Data.Text                       as T
import           Data.Text.Encoding                    (decodeUtf8With,
                                                        encodeUtf8)
import           Data.Text.Encoding.Error              (ignore)
import           Data.Time                       (UTCTime, formatTime)

import           Graphics.Barcode.Code128
import           Graphics.PDF

import           System.Locale                   (defaultTimeLocale)


import           Application.Star.Ballot
import           Application.Star.BallotStyle
import           Application.Star.HashChain
import           Application.Star.SerializableBS (fromSB)

import           Application.StarTerminal.State

import Debug.Trace

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
  { _esSig             :: Encrypted (BallotId, RaceSelection)
  , _esRace            :: Race
  , _esOption          :: Option
  }
$(makeLenses ''ElaboratedSelection)

-- | All the data necessary about a ballot to print it on paper
data ElaboratedBallot = ElaboratedBallot
  { _ebId          :: BallotId
  , _ebCastingTime :: UTCTime
  , _ebPublicHash  :: PublicHash
  , _ebSelections  :: [ElaboratedSelection]
  }
$(makeLenses ''ElaboratedBallot)

elabBallot :: PublicKey -> Ballot -> BallotId -> BallotStyle -> EncryptedRecord -> UTCTime -> ElaboratedBallot
elabBallot pk ballot bid bs enc t =
  ElaboratedBallot bid t (view zp enc)
    [ ElaboratedSelection (encryptRace pk bid rs) r opt
    | rs@(RaceSelection (k, sel)) <- races ballot
    , let Just (_, rid) = fromKey k
    , let Just r = bRace rid bs
    , let Just opt = option sel r
    ]



-- | Draw barcodes and text for a single ballot selection - this is a
-- component of c_bid. Returns the lower-right coordinates.
drawSelection :: ElaboratedSelection -> Point -> Draw Point
drawSelection (ElaboratedSelection sig race opt) start@(x :+ y) = do
    displayFormattedText
        (Rectangle (start - (0 :+ 2*textHeight)) (start + (400 :+ 0)))
        NormalParagraph ballotIdStyle $ do
     paragraph . txt $ "Race: " ++ T.unpack (view rDescription race)
     paragraph . txt $ "Encrypted vote: " ++ sigString
    fromRight $ drawBarcode barcodeConfig sigString (start - (0 :+ 2*textHeight))


    let start' = start - (0 :+ (1.5*textHeight + height barcodeConfig))
    corner <- fromRight $
                drawBarcode barcodeConfig (T.unpack $ view oId opt)
                  (start' - (0 :+ textHeight))
    displayFormattedText
        (Rectangle (corner - (0 :+ textHeight)) (corner + (400 :+ 0)))
        NormalParagraph ballotIdStyle $ do
      paragraph . txt $ "Vote: " ++ T.unpack (view oName opt) ++ maybe "" ((++")") . (" ("++) . T.unpack) (view oParty opt)
    return $ start + (0 :+ (0 - 3*textHeight - 2*height barcodeConfig))
  where barcodeConfig = BarcodeConfig { height = 20, barWidth = 1.0 }
        textHeight = 25
        fromRight (Right x) = x
        Encrypted sigBS = sig
        sigString = T.unpack . decodeUtf8With ignore . toStrict . fromSB $ sigBS

-- | Draw the ballot ID at the top of the page
drawBallotId :: BallotId -> Draw ()
drawBallotId (BallotId bid) = do
  let barcodeStartCorner = (30 :+ (fromIntegral ballotHeight - 30 - (height ballotBarcodeConfig)))
  (_ :+ y) <- case drawBarcode ballotBarcodeConfig (T.unpack bid) barcodeStartCorner of
               Left err -> fail (show err)
               Right draw -> draw
  let idTextRect = Rectangle (35                              :+ (y - height ballotBarcodeConfig - 15))
                             ((fromIntegral ballotWidth - 30) :+ (y - height ballotBarcodeConfig - 5))
  displayFormattedText idTextRect NormalParagraph ballotIdStyle $ do
    paragraph . txt $ "Ballot ID: " <> T.unpack bid

-- | Generate a paper ballot and voter reciept as a PDF
paperBallot :: MonadIO m => Ballot -> BallotId -> BallotStyle -> EncryptedRecord -> Terminal -> UTCTime -> m ByteString
paperBallot ballot bid style enc term t = do
  let elaboratedBallot = elabBallot (view pubkey term) ballot bid style enc t
  let rect = PDFRect 0 0 ballotWidth ballotHeight
  liftIO $ pdfByteString standardDocInfo { compressed = False } rect $ do
    ballotPage <- addPage Nothing
    void . drawWithPage ballotPage $ do
      drawBallotId bid
      foldrM drawSelection (20 :+ 550 ) (view ebSelections elaboratedBallot)
    receiptPage <- addPage Nothing
    drawWithPage receiptPage (receiptPageContents (view tId term) t hashView)
  where (PublicHash hash) = view zp enc
        hashView = take 20 . T.unpack . decodeUtf8With ignore . toStrict . Base16.encode . fromSB $ hash

