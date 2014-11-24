module StarVote.Crypto.Types where

import Data.Array (Array)

data TEGParams
  = TEGParams {
      tegOrder     :: Integer,
      tegGenerator :: Integer,
      tegTrustees  :: Integer,
      tegThreshold :: Integer
  }

data TEGPublicKey = TEGPublicKey TEGParams Integer

data TEGPrivateKey = TEGPrivateKey TEGParams Integer

data TEGCipherText = TEGCipherText Integer Integer

data Shares = Shares (Array Integer Integer)
