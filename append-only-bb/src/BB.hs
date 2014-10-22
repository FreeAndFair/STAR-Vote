{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module BB where

import BB.Time

import Crypto.Hash
import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Generate
import Crypto.PubKey.HashDescr
import Crypto.Random
import Crypto.Types.PubKey.ECC

import Data.Byteable (toBytes)

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 as UTF8

import Data.Convertible

import Data.List (intersperse)

import Data.Monoid

import Data.Time

import Database.HDBC
import Database.HDBC.Sqlite3

import System.Locale (defaultTimeLocale)


sha256 :: ByteString -> Digest SHA256
sha256 = hash

newtype Key = Key { unKey :: String }

instance Show Key where
  show = show . unKey

instance Convertible Key SqlValue where
  safeConvert = safeConvert . unKey

instance Convertible SqlValue Key where
  safeConvert = fmap Key . safeConvert

instance Convertible Key String where
  safeConvert = return . unKey

instance Convertible String Key where
  safeConvert = return . Key





