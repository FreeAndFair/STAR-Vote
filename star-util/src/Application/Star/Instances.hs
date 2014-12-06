{-# LANGUAGE DeriveDataTypeable,
             DeriveGeneric,
             OverloadedStrings,
             StandaloneDeriving,
             TemplateHaskell
  #-}
module Application.Star.Instances where

import Control.Applicative
import Control.Monad
import Crypto.Hash.CryptoAPI
import Crypto.PubKey.ECC.ECDSA
import Crypto.Random.DRBG
import Crypto.Types.PubKey.ECC
import Data.Binary
import Data.Binary.Put
import Data.Byteable
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toChunks)
import GHC.Generics
import Data.Monoid
import Data.SafeCopy
import Data.Typeable

import qualified BB.DB as BB
import qualified BB.Protocol as BB
import qualified Crypto.Random.DRBG.HMAC as HMAC

deriving instance Typeable SHA512
deriving instance Typeable BB.Message
deriving instance Typeable BB.NewMessage
deriving instance Typeable BB.Author
join <$> mapM (deriveSafeCopy 0 'base)
  [ ''HMAC.State, ''SHA512, ''GenError
  , ''PublicKey, ''PrivateKey, ''Signature
  , ''BB.Message, ''BB.Author, ''BB.NewMessage, ''BB.Signed
  , ''Point, ''Curve, ''CurveBinary, ''CurveCommon, ''CurvePrime
  ]

deriving instance Generic Point
deriving instance Generic PublicKey
deriving instance Generic Curve
deriving instance Generic CurveBinary
deriving instance Generic CurvePrime
deriving instance Generic CurveCommon
instance Binary Point
instance Binary PublicKey
instance Binary Curve
instance Binary CurveBinary
instance Binary CurvePrime
instance Binary CurveCommon

instance Byteable Point where
  toBytes PointO = "\0"
  toBytes (Point a b) = "\1" <> strictPut (a, b)

instance Byteable PublicKey where
  toBytes (PublicKey curve q) = toBytes (curve, q)

instance Byteable Curve where
  toBytes (CurveF2m binary) = "\0" <> toBytes binary
  toBytes (CurveFP  prime ) = "\1" <> toBytes prime

instance Byteable CurveBinary where toBytes (CurveBinary n c) = toBytes (n, c)
instance Byteable CurvePrime  where toBytes (CurvePrime  n c) = toBytes (n, c)
instance Byteable CurveCommon where
  toBytes (CurveCommon a b g n h) = mconcat [toBytes a, toBytes b, toBytes g, toBytes n, toBytes h]

instance Byteable Integer where toBytes = strictPut

strictPut :: Binary a => a -> ByteString
strictPut = mconcat . toChunks . runPut . put
