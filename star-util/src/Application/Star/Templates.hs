{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Application.Star.Templates where

import Control.Lens (view)
import Control.Lens.TH

import Data.Monoid

import qualified Data.Text as T

import Text.Blaze.Html5 hiding (map, p)
import Text.Blaze.Html5.Attributes hiding (id, method)
import qualified Text.Blaze.Html5 as H

data Page = Page
  { _pageTitle :: T.Text
  , _pageContents :: Html
  , _pageJavascriptIncludes :: [T.Text]
  , _pageCSSIncludes :: [T.Text]
  }

$(makeLenses ''Page)

blankPage :: Page
blankPage = Page mempty mempty mempty mempty

pageHtml :: Page -> Html
pageHtml p =
  docTypeHtml $ do
    H.head $ do
      H.title (toHtml (view pageTitle p))
      mconcat [ link ! rel "stylesheet"
                     ! type_ "text/css"
                     ! href (toValue css)
              | css <- view pageCSSIncludes p
              ]
      mconcat [ script ! src (toValue js)
                       $ mempty
              | js <- view pageJavascriptIncludes p
              ]
    body $ do
      h1 (toHtml (view pageTitle p))
      view pageContents p
