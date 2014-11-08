{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Application.Star.Voter where

import Control.Lens.TH

import Data.SafeCopy
import Data.Typeable

import qualified Data.Text as T

data Voter = Voter
  { _voterName :: T.Text
  , _voterAddress :: T.Text
  }
  deriving (Read, Show, Typeable)
$(deriveSafeCopy 0 'base ''Voter)
$(makeLenses ''Voter)

data VoterStatus = Voted | Hasn't deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)

$(deriveSafeCopy 0 'base ''VoterStatus)
