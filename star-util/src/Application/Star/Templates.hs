{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Application.Star.Templates where

import Control.Lens (set, view)
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


-- | A page for STAR-vote interfaces with some common defaults
starPage :: Page
starPage = Page mempty
                mempty
                -- TODO: serve this stuff locally!
                [ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/js/bootstrap.min.js" ]
                [ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"
                , "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap-theme.min.css"
                ]

starPageWithContents :: T.Text -> Html -> Page
starPageWithContents title body =
  set pageTitle title . set pageContents body $ starPage

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
