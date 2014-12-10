{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}
module StarVote.Crypto.Types where

import Data.Aeson.TH
import Data.Array (Array, listArray)
import Data.Binary
import Data.List
import Data.Map as M
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

newtype Shares = Shares (Map Integer Integer)
  deriving (Eq, Ord, Read, Show, Typeable)

fromList = Shares . M.fromList
