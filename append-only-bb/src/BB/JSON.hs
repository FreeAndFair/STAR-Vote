{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module BB.JSON where

import Control.Applicative

import Crypto.PubKey.ECC.Generate
import Crypto.Random
import Crypto.Types.PubKey.ECC
import Crypto.Types.PubKey.ECDSA

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

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
