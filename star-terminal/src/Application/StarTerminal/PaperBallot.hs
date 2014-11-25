{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module Application.StarTerminal.PaperBallot (paperBallot) where

import           Control.Arrow
import           Control.Lens                    (view)
import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO, liftIO)

import           Data.ByteString.Lazy            (ByteString, toStrict)
import qualified Data.Map                        as M
import           Data.Monoid
import qualified Data.Text                       as T
import           Data.Text.Encoding                    (decodeUtf8With,
                                                        encodeUtf8)
import           Data.Text.Encoding.Error              (ignore)
import           Data.Time                       (UTCTime, formatTime)

import           Graphics.PDF

import           System.Locale                   (defaultTimeLocale)


import           Application.Star.Ballot
import           Application.Star.BallotStyle
import           Application.Star.HashChain
import           Application.Star.SerializableBS (fromSB)

import           Application.StarTerminal.State

receiptFont :: PDFFont
receiptFont = PDFFont Helvetica 16

receiptStyle :: StandardStyle
receiptStyle = Font receiptFont black black


-- | Generate a paper ballot and voter reciept as a PDF
paperBallot :: MonadIO m => Ballot -> BallotStyle -> EncryptedRecord -> Terminal -> UTCTime -> m ByteString
paperBallot ballot style voted term t = liftIO $ do
  let rect = PDFRect 0 0 700 700 -- TODO compute size - potentially for each page
  pdfByteString standardDocInfo { compressed = False } rect $ do
    ballotPage <- addPage Nothing
    void . drawWithPage ballotPage $ do
      displayFormattedText (Rectangle (10 :+ 10) (690 :+ 690))  NormalParagraph receiptStyle $ do
        paragraph . txt $ "Selections:"
        mapM (paragraph . txt . ("   " ++)) (ballotText ballot style)

    receiptPage <- addPage Nothing
    drawWithPage receiptPage $ do
      displayFormattedText (Rectangle (10 :+ 10) (690 :+ 690))  NormalParagraph receiptStyle $ do
        paragraph . txt $ "Terminal ID: " ++ (T.unpack . decodeUtf8With ignore . toStrict . fromSB) termId
        paragraph . txt $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t
        paragraph . txt $ "Confirmation code: " ++ (show . fromSB) hash
  where (TerminalId termId) = view tId term -- the unique ID of the voting terminal
        (PublicHash hash) = view zp voted
--        bid    = view bId --voted -- the unique ID of the ballot to print

-- | Generate a human-readable summary of a ballot
ballotText :: Ballot -- ^ The marked ballot selections
           -> BallotStyle -- ^ The specification of available races
           -> [String]
ballotText (Ballot selections) style = [ desc <> ": " <> selected k <> "\n"| (k, desc) <- keys ]
  where
    -- Each race in the ballot style gives rise to a key used to look
    -- up the selection in the ballot record. Here, we compute these
    -- keys and retrieve their associated human-readable names.
    keys = map (key style &&& T.unpack . view rDescription) (view bRaces style)
    selected k = maybe "No selection" T.unpack $ M.lookup k selections

