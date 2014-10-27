{-| Ensure a consistent treatment of time (when entered by BB users)
by providing a canonical mapping to and from strings.
-}

module BB.Time where

import Data.Byteable

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Data.Monoid

import Data.Time

import System.Locale (defaultTimeLocale)

-- | Canonical time serialization to readable strings
timeString :: UTCTime -> String
timeString = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | Canonical time deserialization from readable strings
stringTime :: String -> Maybe UTCTime
stringTime = parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"


instance Byteable UTCTime where
  toBytes = mconcat . BSL.toChunks . UTF8.fromString . timeString

