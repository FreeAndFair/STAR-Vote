{-# LANGUAGE ConstraintKinds,
             DeriveDataTypeable,
             FlexibleContexts,
             FlexibleInstances,
             OverloadedStrings,
             Rank2Types,
             TemplateHaskell,
             TypeFamilies
 #-}
module Application.Star.StarController where

import Application.Star.Ballot
import Application.Star.BallotStyle
import Application.Star.HashChain
import Application.Star.ID
import Application.Star.SerializableBS
import Application.Star.Templates
import Application.Star.Util hiding (method)
import Application.Star.CommonImports
import Control.Arrow
import Control.Concurrent
import Control.Lens
import Data.Acid
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import Data.Char
import Data.List (isSuffixOf)
import Data.List.Split
import Data.Maybe
import Data.SafeCopy
import Network.HTTP.Client hiding (method)
import Network.HTTP.Client.TLS
import System.Environment
import System.Random

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html5 ((!))

import qualified Data.HashMap.Lazy as HM
import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP

import Paths_star_controller

-- entry points:
-- GET  generateCode
--      input: none
--      output: a Web form for the poll worker to scan the sticker
-- POST generateCode
-- 	input:  POST body parameter named "style" containing UTF-8 encoded text
-- 	        specifying a ballot style (which will be passed off to the voting
-- 	        terminals)
-- 	output: a ballot code
-- POST fillOut
-- 	input:  POST body containing a JSON-encoded EncryptedRecord describing a
-- 	        ballot; this is automatically called by the voting terminal
-- 	output: none
-- POST cast, POST spoil
-- 	input:  POST body parameter named "bcid" containing UTF-8 encoded ballot
-- 	        casting ID to be marked as cast or spoiled
-- 	output: none
-- GET ballotBox
-- 	input:  none
-- 	output: all filled out ballots as reported by voting terminals
-- POST registerTerminal
--      input: POST body parameter named "url" containing UTF8-encoded URL to add
--             to the broadcast list
--      output: none


-- An example of using the controller and terminal together on one machine
-- might look like this (assuming the appropriate executables are in your
-- PATH):
--
-- # environment setup for star-terminal elided; see star-terminal/start.sh for
-- # an example
--
-- STAR_POST_VOTE_URL=localhost:8000 star-terminal -p 8001 &
--
-- STAR_POST_BALLOT_CODE_URLS=localhost:8001 star-controller -p 8000 &
--
-- curl -X POST localhost:8000/generateCode -d style=oregon-2014
--
-- # visit localhost:8001/ballots in your browser and type in the code printed
-- # by the previous curl command; after being told you voted, you can then...
--
-- # visit localhost:8000/ballotBox in your browser

$(deriveSafeCopy 0 'base ''StdGen)

data ControllerState = ControllerState
  { _seed :: StdGen
  , _broadcastURLs :: [String]
  , _ballotStyles :: Set BallotCode
  -- ballotBox invariant: the bcid in the EncryptedRecord matches the key it's filed under in the Map
  , _ballotBox :: Map BallotCastingId (BallotStatus, EncryptedRecord)
  }
$(deriveSafeCopy 0 'base ''ControllerState)
$(makeLenses ''ControllerState)

instance ToJSON v => ToJSON (Map BallotCastingId v) where
  toJSON m = Object $ HM.fromList [(k, toJSON v) | (BallotCastingId k, v) <- M.toList m]





------------------------------
-- Controller state operations
------------------------------

-- up here due to TH ordering restriction
-- | Run a lens over a state
state' :: MonadState s m => Lens s s t t -> (t -> (a, t)) -> m a
state' l f = state (\s -> second (flip (set l) s) (f (view l s)))

-- | generateCode generates a fresh code by first trying a few random codes; if
-- that doesn't pan out, it searches all possible codes for any it could use
generateCode :: Update ControllerState (Either Text BallotCode)
generateCode = do code <- randomCode retries
                  case code of
                    Just c  -> return (Right c)
                    Nothing -> minimalCode
  where retries = 20 -- magic number picked out of a hat

-- | Adds a new random code, with n retries
randomCode :: Integer -> Update ControllerState (Maybe BallotCode)
randomCode n
  | n > 0 = do
    c       <- state' seed random
    success <- state' ballotStyles (registerCode c)
    if success then return (Just c) else randomCode (n-1)
  | otherwise = return Nothing

minimalCode :: Update ControllerState (Either Text BallotCode)
minimalCode = 
  do db <- view ballotStyles <$> get
     case S.minView (S.difference allCodes db) of
       Just (code, _) -> do modify (over ballotStyles (S.insert code))
                            return (Right code)
       Nothing        -> return (Left "all ballot codes in use")

-- | All possible ballot codes
allCodes :: Set BallotCode
allCodes = S.fromList [minBound..maxBound]

-- | Given a ballot code and a set of ballot codes, add the code and
-- return whether it was not previously in the set.
registerCode :: BallotCode -> Set BallotCode -> (Bool, Set BallotCode)
registerCode code db
  | not (code `S.member` db) = (True, S.insert code db)
  | otherwise = (False, db)


reseed :: StdGen -> Update ControllerState ()
reseed s = modify (set seed s)

getBroadcastURLs :: Query ControllerState [String]
getBroadcastURLs = view broadcastURLs <$> ask

fillOut :: EncryptedRecord -> Update ControllerState ()
fillOut ballot =
  -- TODO: is it okay to always insert? do we need to check that it
  -- isn't there first or something?
  modify (over ballotBox (M.insert (view bcid ballot) (Unknown, ballot)))

setUnknownBallotTo :: BallotStatus -> BallotCastingId -> Update ControllerState (Either Text ())
setUnknownBallotTo status bcid =
  do st <- get
     case view (ballotBox . at bcid) st of
       Just (Unknown, record) ->
         modify (over ballotBox (M.insert bcid (status, record))) >> return (Right ())
       Just (status', record) ->
         return . Left $ T.pack (show bcid) <> " was already " <> T.pack (map toLower (show status'))
       _ -> return . Left $ "Unknown " <> T.pack (show bcid)

readEntireBallotBox :: Query ControllerState (Map BallotCastingId (BallotStatus, EncryptedRecord))
readEntireBallotBox = view ballotBox <$> ask

addURL :: String -> Update ControllerState ()
addURL url = modify $ over broadcastURLs (url:)

$(makeAcidic ''ControllerState [ 'generateCode
                               , 'minimalCode
                               , 'reseed
                               , 'getBroadcastURLs
                               , 'fillOut
                               , 'setUnknownBallotTo
                               , 'readEntireBallotBox
                               , 'addURL
                               ])

---------------------
-- I/O and interfaces
---------------------



main :: IO ()
main = do
  seed <- getStdGen
  filename <- liftIO $ getDataFileName "controllerState"
  st <- liftIO $ openLocalStateFrom filename (ControllerState seed [] def def)
  update st (Reseed seed)
  statefulErrorServe controller st

controller :: (MonadError Text m, MonadAcidState ControllerState m, MonadSnap m) => m ()
controller = route $
  [ ("generateCode",
     method POST
      (do styleID <- decodeParam rqPostParams "style"
          code    <- doUpdate GenerateCode
          case code of
            Left err -> throwError err
            Right c -> do broadcast c styleID
                          writeShow c) <|>
     method GET
       (render . pageHtml . starPageWithContents "Vote!" $ do
          H.p "Scan your barcode here:"
          H.form ! A.method "POST" $ do
            H.input ! A.id "sticker" ! A.name "style" ! A.type_ "text"
            H.input ! A.type_ "submit")
    )
  , ("fillOut",
     method POST $
       do ballot <- readJSONBody
          doUpdate (FillOut ballot)
    )
  , ("cast",
     method POST $
       do castingID <- BallotCastingId <$> readBodyParam "bcid"
          res <- doUpdate $ SetUnknownBallotTo Cast castingID
          case res of
            Left err -> throwError err
            Right () -> return ()
    )
  , ("spoil",
     method POST $
       do castingID <- BallotCastingId <$> readBodyParam "bcid"
          res <- doUpdate $ SetUnknownBallotTo Spoiled castingID
          case res of
            Left err -> throwError err
            Right () -> return ()
    )
  , ("ballotBox",
     method GET $
       do v <- doQuery ReadEntireBallotBox
          writeLBS . encode $ v
    )
  , ("registerTerminal",
     method POST $
       do url <- maybe (error "Required parameter 'url' not present")
                       Char8.unpack <$> getPostParam "url"
          doUpdate (AddURL url)
    )
  -- TODO: provisional casting
  ]



broadcast :: (MonadAcidState ControllerState m, MonadError Text m, MonadSnap m)
          => BallotCode -> BallotStyleId -> m ()
broadcast code styleID =
  do bases <- doQuery GetBroadcastURLs
     urlRequests <- mapM (errorT . parseUrl . urlFor) bases
     -- for now, no error-handling of any kind
     mapM_ (liftIO . forkIO . void . post) urlRequests
  where
    urlFor baseURL = baseURL <> (if "/" `isSuffixOf` baseURL then "" else "/") <>
                     T.unpack styleID <> "/codes/" <> show code
    errorT (Left  e) = throwError . T.pack . show $ e
    errorT (Right v) = return v
    post r = withManager tlsManagerSettings (httpNoBody r { HTTP.method = "POST" })




