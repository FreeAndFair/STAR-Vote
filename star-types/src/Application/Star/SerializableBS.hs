{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Application.Star.SerializableBS where

import           Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON)
import           Data.Binary (Binary)
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid (Monoid)
import           Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

newtype SerializableBS = SB ByteString
  deriving (Eq, Ord, Read, Show, Binary, Monoid)

instance ToJSON SerializableBS where
  toJSON (SB bs) = toJSON (decodeUtf8 (B64.encode bs))

instance FromJSON SerializableBS where
  parseJSON v = do
    txt <- parseJSON v
    case B64.decode (encodeUtf8 txt) of
      Right bs -> return $ SB bs
      Left err -> fail err
