{-# LANGUAGE OverloadedStrings #-}

module Application.StarInit.Controller where

import Application.Star.Templates
import Application.Star.Util
import Application.StarInit.View
import Control.Lens
import Data.ByteString (ByteString)
import Data.Char
import Data.Monoid
import Data.Text (Text)
import Snap
--import System.Posix.Env.ByteString
import System.Process
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as Latin1
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

initTemplate :: Page -> Html
initTemplate p =
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
      meta ! charset "utf-8"
      meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    body $ do
      H.div ! class_ "container" $ do
        h1 $ do
          toHtml (view pageTitle p)
        view pageContents p

writePage :: MonadSnap m => Text -> Html -> m ()
writePage title body = render . initTemplate $ starPageWithContents title body

getTerminalPublicKey :: MonadSnap m => m ()
getTerminalPublicKey = writePage "Election Key Input" getTerminalPublicKeyView

-- TODO: logging
startSubsystem :: MonadSnap m => String -> Int -> m ()
startSubsystem prog port = liftIO $ do
  createProcess (proc prog ["-b", "::", "-p", show port])
  return ()

startDefaultStarTerminal :: MonadSnap m => Int -> ByteString -> m ()
startDefaultStarTerminal port key = do
  liftIO $ do
    setEnv "STAR_PUBLIC_KEY"         key                                                                True
    setEnv "STAR_TERMINAL_ID"        "9b59bbb0-5a45-11e4-8ed6-0800200c9a66"                             True
    setEnv "STAR_INIT_PUBLIC_HASH"   "fcc578c708d198baecd7024be8dae72639d1560f3eaeac24f651e7be69c2b886" True
    setEnv "STAR_INIT_INTERNAL_HASH" "7ab9c86899a0007945ee13cd00b565b35d6dc3807ac9eaf08b9729b3a1267ffd" True
    setEnv "STAR_PUBLIC_SALT"        "7ff1adc82440511d520c1116df34fee4e88003ccc3e73ac95f29958df06eefce" True
    setEnv "STAR_POST_VOTE_URL"      "http://localhost:8003/fillOut"                                    True
    setEnv "STAR_REGISTER_URL"       "http://localhost:8003/registerTerminal"                           True
  startSubsystem "star-terminal" port

getTerminalParams :: MonadSnap m => ByteString -> m (ByteString, Int)
getTerminalParams url = do
  mkey <- getParam "publickey"
  port <- maybe "8004" (filter isDigit . Latin1.unpack) <$> getParam "port"
  case mkey of
    Nothing  -> redirect url
    Just key -> return (key, read port)

startTerminal :: MonadSnap m => m ()
startTerminal = do
  (key, port) <- getTerminalParams "/startTerminal"
  startDefaultStarTerminal port key
  writePage "Terminal Started" (terminalStartedView (show port))

instructions :: MonadSnap m => m ()
instructions = writePage "Election Ritual" instructionsView

startBB :: MonadSnap m => m ()
startBB = do
  startSubsystem "bbserver" 8000
  writePage "Bulletin Board Status" (p "Bulletin board subsystem started")

startLEO :: MonadSnap m => m ()
startLEO = do
  startSubsystem "star-keygen"   8001
  startSubsystem "star-voter-db" 8002
  writePage "LEO Subsystems Status" (p "LEO subsystems started")

startController :: MonadSnap m => m ()
startController = do
  startSubsystem "star-controller" 8003
  writePage "Controller Subsystem Status" (p "Controller subsystem started")

startPollBooth :: MonadSnap m => m ()
startPollBooth = do
  (key, port) <- getTerminalParams "/subsystems/pollbooth/start"
  startDefaultStarTerminal port key
  writePage "Polling Booth Terminal Status" (p "Polling booth terminal started")
