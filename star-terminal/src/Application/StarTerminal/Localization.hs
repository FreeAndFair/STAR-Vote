{-# LANGUAGE OverloadedStrings #-}
module Application.StarTerminal.Localization where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as Map

type TranslationKey = Text
newtype Translations = Translations (Map TranslationKey Text)

translations :: [(Text, Text)] -> Translations
translations = Translations . Map.fromList

localize :: TranslationKey -> Translations -> Text
localize k ts = Map.findWithDefault (T.concat ["[", k, "]"]) k (m ts)
  where
    m (Translations m') = m'
