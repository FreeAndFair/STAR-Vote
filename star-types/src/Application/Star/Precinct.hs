{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Application.Star.Precinct where

import Data.SafeCopy
import Data.Typeable (Typeable)

data Precinct = Precinct -- TODO
  deriving (Typeable)

$(deriveSafeCopy 0 'base ''Precinct)
