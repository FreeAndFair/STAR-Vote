{-# LANGUAGE OverloadedStrings #-}

module Main where

import Application.StarInit.Controller
import Snap

main = quickHttpServe . route $
  [ ("/startTerminal",               method GET  getTerminalPublicKey)
  , ("/startTerminal",               method POST startTerminal)
  , ("/",                            method GET  instructions)
  , ("/subsystems/BB/start",         method GET  startBB)
  , ("/subsystems/LEO/start",        method GET  startLEO)
  , ("/subsystems/controller/start", method GET  startController)
  , ("/subsystems/terminal/start",   method GET  getTerminalPublicKey)
  , ("/subsystems/terminal/start",   method POST startPollBooth)
  ]
