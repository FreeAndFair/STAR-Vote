{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import           Application.StarTerminal.Controller

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    -- ifTop (formHandler) <|>
    route [ ("ballot/:ballotId/step/:stepId", ballotStepHandler)
          , ("ballot/:ballotId", ballotHandler)
          ] <|>
    dir "static" (serveDirectory "static")
