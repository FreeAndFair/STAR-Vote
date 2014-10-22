{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module BB.JSON where

import BB.Protocol

import Control.Applicative

import Crypto.PubKey.ECC.Generate
import Crypto.Random
import Crypto.Types.PubKey.ECC
import Crypto.Types.PubKey.ECDSA

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.UTF8 as UTF8

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T


$(deriveJSON defaultOptions ''Point)
$(deriveJSON defaultOptions ''CurvePrime)
$(deriveJSON defaultOptions ''CurveCommon)
$(deriveJSON defaultOptions ''CurveBinary)
$(deriveJSON defaultOptions ''Curve)
$(deriveJSON defaultOptions ''PublicKey)
$(deriveJSON defaultOptions ''PrivateKey)


instance ToJSON Signature where
  toJSON (Signature r s) = Object . HM.fromList $
                             [ (T.pack "r", toJSON r)
                             , (T.pack "s", toJSON s)
                             ]

instance FromJSON Signature where
  parseJSON (Object v) = Signature <$> v .: "r" <*> v .: "s"
  parseJSON x = fail $ "Signatures must be represented as JSON objects, got " ++ show x

instance ToJSON BS.ByteString where
  toJSON bs = toJSON . UTF8.toString $ Base64.encode bs

instance FromJSON BS.ByteString where
  parseJSON (String str) = fmap (Base64.decodeLenient . UTF8.fromString) . parseJSON $ String str
  parseJSON other = fail $ "Must be a base64-encoded string, got " ++ show other


$(deriveJSON defaultOptions ''Signed)
$(deriveJSON defaultOptions ''CurrentHash)

