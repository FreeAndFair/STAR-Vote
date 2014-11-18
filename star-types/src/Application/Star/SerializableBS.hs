{-# LANGUAGE GeneralizedNewtypeDeriving,
             TemplateHaskell #-}

{-|
Module      : Application.Star.SerializableBS
Description : newtype wrapper for ByteStrings with Aeson instances

Provides the type `SerializableBS`,
which allows ByteString values to be serialized to and from JSON.
Values are encoded using base-16 to match the format for terminal configuration.
 -}
module Application.Star.SerializableBS where

import           Control.Applicative ((<$>))
import           Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON)
import           Data.Binary.Get (getRemainingLazyByteString)
import           Data.Binary.Put (putLazyByteString)
import           Data.Binary (Binary, get, put)
import qualified Data.ByteString.Base16.Lazy as B16
import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid (Monoid)
import           Data.SafeCopy (deriveSafeCopy, base)
import           Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

newtype SerializableBS = SB { fromSB :: ByteString }
  deriving (Monoid)

$(deriveSafeCopy 0 'base ''SerializableBS)

instance Binary SerializableBS where
  put (SB bs) = putLazyByteString bs
  get = SB <$> getRemainingLazyByteString

instance ToJSON SerializableBS where
  toJSON (SB bs) = toJSON (decodeUtf8 (B16.encode bs))

instance FromJSON SerializableBS where
  parseJSON v = do
    txt <- parseJSON v
    let (bs, _) = B16.decode (encodeUtf8 txt)
    return $ SB bs
