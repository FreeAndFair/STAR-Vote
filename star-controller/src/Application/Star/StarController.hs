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
import Application.Star.Util hiding (method, ballotStyles)
import Application.Star.CommonImports
import Control.Arrow
import Control.Concurrent
import Control.Lens
import Data.Acid
import Data.Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as Char8
import Data.Char
import Data.Either
import Data.List (isSuffixOf, sortBy)
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.String
import Data.Text.Encoding (decodeUtf8)
import Data.SafeCopy
import Network.HTTP.Client hiding (method)
import Network.HTTP.Client.TLS
import Snap.Util.FileServe
import StarVote.Crypto.Types as TEG
import StarVote.Crypto.ThresholdElGamal
import System.Environment
import System.Random

import qualified Data.Map as M
import qualified Data.MultiSet as Bag
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

extractStyle :: Text -> Text
extractStyle barcode = maybe barcode id . msum $ map getCode parts

  where parts = T.splitOn ";" barcode
        getCode part | T.isPrefixOf "B=" part = Just (T.drop 2 part)
                     | otherwise              = Nothing


controller :: (MonadError Text m, MonadAcidState ControllerState m, MonadSnap m) => m ()
controller = liftIO (getDataFileName "static") >>= \static ->
  dir "static" (serveDirectory static) <|> route
  [ ("generateCode",
     method POST
      (do styleID <- extractStyle <$> decodeParam rqPostParams "style"
          code    <- doUpdate GenerateCode
          case code of
            Left err -> throwError err
            Right c -> do broadcast c styleID
                          writeShow c) <|>
     method GET
       (page "Vote!" $ do
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
     (method GET $
        page "Cast ballot" $ do
          H.p "Scan the barcode, then place the ballot in the box."
          H.form ! A.method "POST" ! A.class_ "form" $ do
            H.label ! A.for "bcid" $ "Casting ID:"
            H.input ! A.id "bcid" ! A.name "bcid" ! A.type_ "text"
            H.input ! A.type_ "submit" ! A.value "Cast") <|>
     (method POST $
        do castingID <- maybe (error "Required param 'bcid' not present")
                              (BallotCastingId . decodeUtf8) <$> getPostParam "bcid"
           res <- doUpdate $ SetUnknownBallotTo Cast castingID
           case res of
             Left err ->
               page "Vote not registered" $ do
                 H.p $ H.toHtml err
                 H.p $ H.a ! A.href "/cast" $ "Return"
             Right () ->
               page "Thank you for voting" $ do
                 H.p "Your ballot has been cast. Thank you for voting."
                 H.p $ H.a ! A.href "/cast" $ "Return")
    )
  , ("spoil",
     (method GET $
       page "Invalidate Ballot" $ do
         H.p $
           "Scan your barcode to invalidate the ballot. Invalidated (spoiled) " <>
           "ballots can be used to check that the election system is working " <>
           "properly. You can also re-vote after spoiling your ballot."
         H.form ! A.method "POST" ! A.class_ "form" $ do
           H.label ! A.for "bcid" $ "Casting ID:"
           H.input ! A.id "bcid" ! A.name "bcid" ! A.type_ "text"
           H.input ! A.type_ "submit" ! A.value "Invalidate/Spoil") <|>
     (method POST $
        do castingID <- maybe (error "Required param 'bcid' not present")
                              (BallotCastingId . decodeUtf8) <$> getPostParam "bcid"
           res <- doUpdate $ SetUnknownBallotTo Spoiled castingID
           case res of
             Left err ->
               page "Ballot not invalidated" $ do
                 H.p $ H.toHtml err
                 H.p $ H.a ! A.href "/spoil" $ "Return" 
             Right () ->
               page "Ballot invalidated" $ do
                 H.p $ "Your ballot has been invalidated. Please consider saving it " <>
                       "in order to audit the election."
                 H.p $ "Enter the original five-digit voting code at a terminal to " <>
                       "mark another ballot."
                 H.p $ H.a ! A.href "/spoil" $ "Return")
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
  , ("tally", method GET decryptionParametersForm <|>
              method POST (do
      params  <- readBodyParam "params"
      shares  <- forM [1..tegThreshold params] (readBodyParam . shareName)
      ballots <- doQuery ReadEntireBallotBox
      tally params shares ballots
      )
    )
  , ("poolShares", method POST $ do
      encodedPublicKey <- getPostParam "public"
      TEGPublicKey params _ <- case encodedPublicKey of
        Nothing -> throwError "couldn't decode public key"
        Just bs -> return . Binary.decode . BS.fromStrict . B64.decodeLenient $ bs
      sharesForm params
    )
  ]

page :: MonadSnap m => Text -> H.Html -> m ()
page title = render . pageHtml . (pageCSSIncludes <>~ ["static/site.css"]) . starPageWithContents title

decryptionParametersForm :: MonadSnap m => m ()
decryptionParametersForm = page "Vote decryption, part 1/3" $ do
  H.p "Enter the public key to begin."
  H.form ! A.method "POST" ! A.action "poolShares" $ do
    H.input ! A.name "public" ! A.type_ "text"
    H.input ! A.type_ "submit"

-- TODO: Should probably decrypt spoiled ballots and say something one way or
-- another about ballots that are neither spoiled nor cast.
tally :: MonadSnap m => TEGParams -> [(Integer, Integer)] -> Map k (BallotStatus, EncryptedRecord) -> m ()
tally params shares ballotBox = do
  let ballots = [decryptRecord private er | (Cast, er) <- M.elems ballotBox]
      private = TEGPrivateKey params (recoverKeyFromShares params (TEG.fromList shares))
      (failures, successes) = partitionEithers ballots
      summary = M.unionsWith Bag.union [Bag.singleton <$> b | Ballot b <- successes]
  styles <- getBallotStyles
  page "Vote decryption, part 3/3" $ do
    H.h2 "successfully decrypted votes"
    renderOptions $ lookupOptions styles summary
    when (not . null $ failures) $ do
      H.h2 "WARNING! Some errors detected."
      H.ul (mapM_ (H.li . fromString . show) failures)

type TallySummary = (Maybe BallotStyleId, Maybe Race, Maybe Option, Int)

lookupOptions :: BallotStyles
              -> Map BallotKey (Bag.MultiSet Selection)
              -> [TallySummary]
lookupOptions styles summary =
  [ process key selection count
  | (key, bag) <- M.toList summary
  , (selection, count) <- Bag.toOccurList bag
  ] where

  lookupLens lens key list = [v | v <- list, view lens v == key]

  lookupRace :: BallotStyleId -> RaceId -> Maybe Race
  lookupRace bid rid = listToMaybe (lookupLens bId bid styles >>= lookupLens rId rid . view bRaces)

  lookupOption :: Selection -> [Option] -> Maybe Option
  lookupOption selection options = listToMaybe (lookupLens oId selection options)

  process key selection count = let
    mStyleRace = fromKey key
    mStyleId   = fst <$> mStyleRace
    mRace      = mStyleRace >>= uncurry lookupRace
    mOption    = mRace >>= lookupOption selection . view rOptions
    in (mStyleId, mRace, mOption, count)

renderOptions :: [TallySummary] -> H.Html
renderOptions summary = H.div ! A.class_ "tally" $ H.table $ do
  H.thead . H.tr $ do
    H.th "Ballot style"
    H.th "Race"
    H.th "Candidate"
    H.th "Vote tally"
  H.tbody . mapM_ renderOption . sortBy ordering $ summary
  where
  ordering = comparing (\(bid,_,_,_) -> bid)
          <> comparing (\(_,race,_,_) -> view rDescription <$> race)
          <> comparing (\(_,_,_,count) -> count)
          <> comparing (\(_,_,option,_) -> view oName <$> option)

renderOption :: TallySummary -> H.Html
renderOption (mStyleId, mRace, mOption, count) = H.tr $ do
  warnNothing badStyle  mStyleId H.toHtml
  warnNothing badRace   mRace    (H.toHtml . view rDescription)
  warnNothing badOption mOption  (H.toHtml . view oName)
  H.td . fromString . show $ count
  where
  warnNothing msg val f = maybe (H.td ! A.class_ "warning" $ msg)
                                (H.td . f)
                                val
  badStyle  = "Ill-formed ballot style"
  badRace   = "Unknown race"
  badOption = "No matching candidate"

sharesForm params = page "Vote decryption, part 2/3" $ do
  H.p "Each trustee should enter a share below."
  H.form ! A.method "POST" ! A.action "tally" $ do
    forM [1..tegThreshold params] $ \i -> H.input ! A.name (shareName i) ! A.type_ "text"
    H.input ! A.name "params" ! A.type_ "hidden" ! (A.value . fromString . show) params
    H.input ! A.type_ "submit" ! A.value "combine shares"

shareName i = fromString ("share" <> show i)


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
