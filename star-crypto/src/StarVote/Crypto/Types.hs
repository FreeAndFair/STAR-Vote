{-# LANGUAGE DeriveDataTypeable #-}
module StarVote.Crypto.Types where

import Data.Array (Array)
import Data.Typeable

data TEGParams
  = TEGParams {
      tegOrder     :: Integer,
      tegGenerator :: Integer,
      tegTrustees  :: Integer,
      tegThreshold :: Integer
  } deriving (Eq, Ord, Read, Show, Typeable)

data TEGPublicKey = TEGPublicKey TEGParams Integer
  deriving (Eq, Ord, Read, Show, Typeable)

data TEGPrivateKey = TEGPrivateKey TEGParams Integer
  deriving (Eq, Ord, Read, Show, Typeable)

data TEGCipherText = TEGCipherText Integer Integer
  deriving (Eq, Ord, Read, Show, Typeable)

data Shares = Shares (Array Integer Integer)
  deriving (Eq, Ord, Read, Show, Typeable)
