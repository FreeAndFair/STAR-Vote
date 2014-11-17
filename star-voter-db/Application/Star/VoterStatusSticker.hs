{-# LANGUAGE OverloadedStrings #-}
module Application.Star.VoterStatusSticker (sticker) where

import Application.Star.BallotStyle
import Application.Star.CommonImports hiding (method)
import Application.Star.ID
import Application.Star.Precinct
import Application.Star.Util
import Application.Star.Voter

import Control.Lens

import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text as T

import Graphics.Barcode.Code128
import Graphics.PDF

import Text.Printf


starBarcodeConfig :: BarcodeConfig
starBarcodeConfig = BarcodeConfig { height = 5 * styleHeight starStyle, barWidth = 0.75 }

starFont :: PDFFont
starFont = PDFFont Helvetica 5

starStyle :: StandardStyle
starStyle = Font starFont black black

stickerTextWidth :: T.Text -> Double
stickerTextWidth = textWidth starFont . toPDFString . T.unpack


-- | Use the most recent precinct data
sticker :: Bool -> Voter -> ID Precinct -> BallotStyleId -> IO LBS.ByteString
sticker provisional v prec ballotstyle =
  do let rect = PDFRect 0 0 200 (ceiling $ height starBarcodeConfig)
     pdfByteString standardDocInfo { compressed = False } rect $ do
       p <- addPage Nothing
       drawWithPage p $ do
         (x :+ y) <- case drawBarcode starBarcodeConfig barcodeContents (0 :+ 0) of
                       Left err -> fail (show err)
                       Right draw -> draw
         displayFormattedText (textBox (x :+ (y - height starBarcodeConfig))) NormalParagraph starStyle $ do
           paragraph . txt $ T.unpack name
           paragraph . txt $ T.unpack meta

  where provisionalMark :: String
        provisionalMark = if provisional
                             then "PROV"
                             else ""
        name = view voterName v
        nameWidth = stickerTextWidth name
        meta = T.pack $ printf "Precinct: %s%03d  Ballot: %s" provisionalMark (getID prec) (T.unpack ballotstyle)
        metaWidth = stickerTextWidth meta

        textAreaWidth = max nameWidth metaWidth

        textBox (x :+ y) = Rectangle (x :+ y) ((x + textAreaWidth) :+ (y + height starBarcodeConfig))

        barcodeContents :: String
        barcodeContents = printf "P=%03d;B=%s" (getID prec) (T.unpack ballotstyle)

