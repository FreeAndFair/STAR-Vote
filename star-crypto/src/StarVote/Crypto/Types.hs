{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}
module StarVote.Crypto.Types where

import Data.Aeson.TH
import Data.Array (Array)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data TEGParams
  = TEGParams {
      tegOrder     :: Integer,
      tegGenerator :: Integer,
      tegTrustees  :: Integer,
      tegThreshold :: Integer
  } deriving (Eq, Ord, Read, Show, Typeable, Generic)
instance Binary TEGParams

data TEGPublicKey = TEGPublicKey TEGParams Integer
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
instance Binary TEGPublicKey

data TEGPrivateKey = TEGPrivateKey TEGParams Integer
  deriving (Eq, Ord, Read, Show, Typeable)

data TEGCipherText = TEGCipherText Integer Integer
  deriving (Eq, Ord, Read, Show, Typeable, Generic)
instance Binary TEGCipherText
deriveJSON defaultOptions ''TEGCipherText

data Shares = Shares (Array Integer Integer)
  deriving (Eq, Ord, Read, Show, Typeable)
