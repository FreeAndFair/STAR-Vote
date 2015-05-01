{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Application.StarTerminal.Localization
Description : Adhoc i18n system

Adhoc i18n system, for use until a real i18n library is incorporated.
 -}
module Application.StarTerminal.Localization where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid

type TranslationKey = Text
newtype Translations = Translations (Map TranslationKey Text)
  deriving Monoid

translations :: [(Text, Text)] -> Translations
translations = Translations . Map.fromList

localize :: TranslationKey -> Translations -> Text
localize k ts = Map.findWithDefault (T.concat ["[", k, "]"]) k (m ts)
  where
    m (Translations m') = m'
