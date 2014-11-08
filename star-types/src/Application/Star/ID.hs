{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Application.Star.ID where

import Control.Arrow
import Control.Applicative
import Data.Aeson

import Data.SafeCopy
import Data.Typeable (Typeable)

newtype ID a = ID { getID :: Integer } deriving (Eq, Ord, ToJSON, FromJSON, Typeable)

$(deriveSafeCopy 0 'base ''ID)

instance Num (ID a) where
  ID a + ID b = ID (a + b)
  ID a * ID b = ID (a * b)
  ID a - ID b = ID (a - b)
  negate (ID a) = ID (negate a)
  abs    (ID a) = ID (abs    a)
  signum (ID a) = ID (signum a)
  fromInteger   = ID

instance Show (ID a) where showsPrec n = showsPrec n . getID
instance Read (ID a) where readsPrec n = fmap (first ID) . readsPrec n
