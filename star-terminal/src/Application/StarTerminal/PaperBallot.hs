{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.PaperBallot (paperBallot) where

import           Control.Arrow
import           Control.Lens                    (view)
import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO, liftIO)

import           Data.ByteString.Lazy            (ByteString, toStrict)
import qualified Data.ByteString.Base16.Lazy     as Base16
import           Data.Foldable                   (find)
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


-- TODO: c_bid must be included!
-- | Draw the contents of the paper ballot, to be inserted into the ballot box
ballotPageContents :: T.Text -> BallotStyle -> Ballot -> Draw ()
ballotPageContents bid style ballot = do
  let barcodeStartCorner = (30 :+ (fromIntegral ballotHeight - 30 - (height ballotBarcodeConfig)))
  (_ :+ y) <- case drawBarcode ballotBarcodeConfig (T.unpack bid) barcodeStartCorner of
               Left err -> fail (show err)
               Right draw -> draw
  let idTextRect = Rectangle (35                              :+ (y - height ballotBarcodeConfig - 15))
                             ((fromIntegral ballotWidth - 30) :+ (y - height ballotBarcodeConfig - 5))
  displayFormattedText idTextRect NormalParagraph ballotIdStyle $ do
    paragraph . txt $ "Ballot ID: " <> T.unpack bid
  let ballotSelectionRect =
        Rectangle (30                              :+ 30)
                  ((fromIntegral ballotWidth - 30) :+ (y - height ballotBarcodeConfig - 25))
  displayFormattedText ballotSelectionRect NormalParagraph receiptStyle $ do
    paragraph . txt $ "Selections:"
    void $ mapM (paragraph . txt . ("   " ++) . T.unpack) (ballotText ballot style)
    paragraph . txt $ "   -----------    "


receiptPageContents :: TerminalId -> UTCTime -> String -> Draw ()
receiptPageContents term t hashView = do
  displayFormattedText (Rectangle (10 :+ 10) (690 :+ 690))  NormalParagraph receiptStyle $ do
    paragraph . txt $ "Terminal ID: " ++ (T.unpack . decodeUtf8With ignore . toStrict . fromSB) termId
    paragraph . txt $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t
    paragraph . txt $ "Confirmation code: " ++ hashView
  where (TerminalId termId) = term


-- | Generate a paper ballot and voter reciept as a PDF
paperBallot :: MonadIO m => Ballot -> BallotId -> BallotStyle -> EncryptedRecord -> Terminal -> UTCTime -> m ByteString
paperBallot ballot (BallotId bid) style voted term t = liftIO $ do
  let rect = PDFRect 0 0 ballotWidth ballotHeight
  pdfByteString standardDocInfo { compressed = False } rect $ do
    ballotPage <- addPage Nothing
    void $ drawWithPage ballotPage (ballotPageContents bid style ballot)

    receiptPage <- addPage Nothing
    drawWithPage receiptPage (receiptPageContents (view tId term) t hashView)
  where (PublicHash hash) = view zp voted
        hashView = take 20 . T.unpack . decodeUtf8With ignore . toStrict . Base16.encode . fromSB $ hash
--        bid    = view bId --voted -- the unique ID of the ballot to print

-- | Generate a human-readable summary of a ballot
ballotText :: Ballot -- ^ The marked ballot selections
           -> BallotStyle -- ^ The specification of available races
           -> [T.Text]
ballotText (Ballot selections) style = [ desc <> ": " <> (selected r k) <> "\n"
                                       | (k, (desc, r)) <- keys
                                       ]
  where
    -- Each race in the ballot style gives rise to a key used to look
    -- up the selection in the ballot record. Here, we compute these
    -- keys and retrieve their associated human-readable names.
    keys :: [(BallotKey, (T.Text, Race))]
    keys = map (key style &&& (view rDescription &&& id)) (view bRaces style)
    selected :: Race -> T.Text -> T.Text
    selected r k = maybe "No selection" id $
                     do selectedId  <- M.lookup k selections
                        let opts = view rOptions r
                        selectedOpt <- find ((== selectedId) . view oId) opts
                        return $ view oName selectedOpt <> maybe "" parens (view oParty selectedOpt)
                          where parens = (" (" <>) . (<> ")")


