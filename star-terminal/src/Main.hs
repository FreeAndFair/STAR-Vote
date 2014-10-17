{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Data.CaseInsensitive (mk)
import           Data.Monoid ((<>))
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Text.Blaze.Html5 (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import           Application.StarTerminal.Localization
import           Application.StarTerminal.View

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (formHandler) <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory "static")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

formHandler :: Snap ()
formHandler = do
  modifyResponse $ setContentType "text/html"
                 . setHeader (mk "Cache-Control") "max-age=0"
  render (page "Test Form" (navbar <> form))
  where form = formView strings

render :: Html -> Snap ()
render = writeLBS . renderHtml

strings :: Translations
strings = translations
  [ ("foo", "bar")
  , ("baz", "nao")
  , ("select_candidate", "Please select a candidate")
  ]
