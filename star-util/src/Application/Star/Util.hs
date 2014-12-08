{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Application.Star.Util (
  -- * AcidState helpers
  MonadAcidState, doQuery, doUpdate,
  errorUpdate, errorUpdateShow, randTrans,
  -- * HTTP and HTML helpers
  errorResponse, writeShow, method,
  statefulErrorServe, statefulErrorServeDef,
  readJSONBody, decodeParam, readParam,
  reportWhere,
  readURIParam, readBodyParam,
  render,
  -- * Global election configuration information
  getBallotStyles, ballotOption
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.CatchIO
import           Control.Monad.CryptoRandom
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Acid                     (AcidState, EventResult,
                                                EventState, QueryEvent,
                                                UpdateEvent, Update,
                                                query, update)
import           Data.Aeson
import           Data.ByteString               (ByteString)
import           Data.CaseInsensitive          (mk)
import           Data.Char
import           Data.Default
import qualified Data.Map                      as M
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding
import           Snap.Core                     hiding (method)
import           Snap.Http.Server              (quickHttpServe)
import           Snap.Iteratee                 (TooManyBytesReadException)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5              (Html)

import           Application.Star.BallotStyle

type MonadAcidState a m = MonadState (AcidState a) m

-- | Run an ACID-state query in a smart enough monad
doQuery :: (MonadIO m, MonadAcidState (EventState event) m, QueryEvent event)
          => event
          -> m (EventResult event)
doQuery e = do st <- get
               liftIO (query st e)

-- | Run an ACID-state update in a smart enough monad
doUpdate :: (MonadIO m, MonadAcidState (EventState event) m, UpdateEvent event)
           => event
           -> m (EventResult event)
doUpdate e = do st <- get
                res <- liftIO (update st e)
                put st
                return res

errorUpdate :: (MonadIO m, MonadAcidState (EventState event) m, UpdateEvent event, EventResult event ~ Either e a, MonadError e' m)
            => (e -> e')
            -> event
            -> m a
errorUpdate f v = doUpdate v >>= either (throwError . f) return

errorUpdateShow :: (MonadIO m, MonadAcidState (EventState event) m, UpdateEvent event, EventResult event ~ Either e a, Show e, MonadError Text m)
                => event -> m a
errorUpdateShow = errorUpdate (T.pack . show)

-- state modifications during failing transactions are not preserved
randTrans :: e ~ GenError => Lens s s g g -> CRand g e a -> Update s (Either e a)
randTrans l action = do
  seed <- gets (view l)
  case runCRand action seed of
    Left e -> return (Left e)
    Right (a, seed') -> modify (set l seed') >> return (Right a)

errorResponse :: MonadSnap m => Text -> m ()
errorResponse err = modifyResponse (setResponseCode 400) >> writeText err

writeShow :: (MonadSnap m, Show a) => a -> m ()
writeShow = writeText . T.pack . show

method :: (MonadError Text m, MonadSnap m) => Method -> m ()
method requested = do
  actual <- rqMethod <$> getRequest
  when (actual /= requested) . throwError
    $  "Bad method, expected " <> (T.pack . show) requested
    <> " but got "             <> (T.pack . show) actual


-- This is a bit of an ugly instance - see
-- http://www.mail-archive.com/haskell-cafe@haskell.org/msg82859.html
-- for details. However, it's necessary for a MonadSnap instance.
instance (MonadCatchIO m) => MonadCatchIO (ExceptT e m) where
  m `catch` f = mapExceptT (\m' -> m' `catch` \e -> runExceptT $ f e) m
  block       = mapExceptT block
  unblock     = mapExceptT unblock

instance (MonadSnap m, Monoid e) => MonadSnap (ExceptT e m) where
  liftSnap = lift . liftSnap

statefulErrorServeDef :: Default s => StateT s (ExceptT Text Snap) a -> IO ()
statefulErrorServeDef m = statefulErrorServe m def

statefulErrorServe :: StateT s (ExceptT Text Snap) a -> s -> IO ()
statefulErrorServe m s = do
  quickHttpServe (do
    v_ <- runExceptT (runStateT m s)
    case v_ of
      Left err -> errorResponse err
      Right _  -> return ())

-- | This defaults to a maximum request body of one megabyte.
readJSONBody :: (MonadSnap m, MonadError Text m, FromJSON a) => m a
readJSONBody = do
  r <- catch (readRequestBody maxRequestSize)
             (\_v -> const (throwError "request too large") (_v :: TooManyBytesReadException))
  case decode r of
    Just v  -> return v
    Nothing -> throwError "error decoding JSON"
  where
    maxRequestSize = 1048576

decodeParam :: (MonadError Text m, MonadSnap m) => (Request -> Params) -> ByteString -> m Text
decodeParam extractParams name = do
  params <- extractParams <$> getRequest
  reportWhere name $ case M.lookup name params of
    Just (bs:_) -> case decodeUtf8' bs of
      Right t -> return t
      _ -> Left "badly encoded"
    _ -> Left "missing"

readParam :: (MonadError Text m, MonadSnap m, Read a) => (Request -> Params) -> ByteString -> m a
readParam extractParams name = do
  text <- decodeParam extractParams name
  reportWhere name $ case reads (T.unpack text) of
    (v, rest):_ | all isSpace rest -> Right v
    _ -> Left "unparseable"

reportWhere :: MonadError Text m => ByteString -> Either Text a -> m a
reportWhere name (Left  s) = throwError (s <> " argument in parameter " <> T.pack (show name))
reportWhere _    (Right v) = return v

readURIParam, readBodyParam :: (MonadError Text m, MonadSnap m, Read a) => ByteString -> m a
readURIParam  = readParam rqQueryParams
readBodyParam = readParam rqPostParams

render :: MonadSnap m => Html -> m ()
render h = do
  modifyResponse $ setContentType "text/html"
                 . setHeader (mk "Cache-Control") "max-age=0"
  writeLBS (renderHtml h)

-- TODO: allow the election to be configured ahead of time!
-- | Get the ballot styles for the current election
-- For now, it's a stub. Should be filled out with something that consults the pre-election configuration.
getBallotStyles :: MonadIO m => m BallotStyles
getBallotStyles = return exampleBallotStyles

-- | Attempt to retrieve a full ballot option, given a ballot style,
-- race ID, and option ID.
ballotOption :: MonadIO m => BallotStyleId -> RaceId -> OptionId -> m (Maybe Option)
ballotOption bsid rid oid = do
  styles <- getBallotStyles
  return $ do chosenStyle <- uniqueElement [ s | s <- styles, view bId s == bsid]
              chosenRace <- uniqueElement [ r | r <- view bRaces chosenStyle, view rId r == rid]
              uniqueElement [o | o <- view rOptions chosenRace, view oId o == oid]

  where uniqueElement :: [a] -> Maybe a
        uniqueElement [x] = Just x
        uniqueElement _   = Nothing

-- | An example ballot style.
-- For now this is the only ballot style that is available to terminals.
-- In the future ballot styles will be provided during configuration,
-- rather than hard-coded.
exampleBallotStyles :: BallotStyles
exampleBallotStyles =
  [ BallotStyle
    { _bId = "oregon-2014"
    , _bRaces =
      [ Race
        { _rDescription = "Oregon Governor"
        , _rId = "gov"
        , _rOptions =
          [ Option "c1" "Aaron Auer"        (Just "Con") (Just "Minister of the Gospel")
          , Option "c2" "Tovia E Fornah"    (Just "Non") (Just "Service")
          , Option "c3" "Paul Grad"         (Just "L")   (Just "Investor")
          , Option "c4" "Chris Henry"       (Just "P")   Nothing
          , Option "c5" "John Kitzhaber"    (Just "Dem") (Just "Governor of Oregon")
          , Option "c6" "Jason Levin"       (Just "Grn") (Just "Cannabis Industry Professional")
          , Option "c7" "Dennis Richardson" (Just "Rep") (Just "Businessman; State Representative")
          ]
        }
      , Race
        { _rDescription = "US Senator"
        , _rId = "senate"
        , _rOptions =
          [ Option "s1" "James E. Leuenberger" (Just "Con") Nothing
          , Option "s2" "Christina Jean Lugo"  (Just "Grn") (Just "Artist, Peace Activist")
          , Option "s3" "Jeff Merkley"         (Just "Dem") (Just "United States Senator")
          , Option "s4" "Mike Montchalin"      (Just "L")   (Just "Candidate/Retired")
          , Option "s5" "Monica Wehby"         (Just "Rep") (Just "Pediatric Neurosurgeon")
          ]
        }
      , Race
        { _rDescription = "US Representative, 3rd District"
        , _rId = "rep_3"
        , _rOptions =
          [ Option "r1" "Earl Blumenauer"  (Just "Dem") (Just "U.S. Congressman")
          , Option "r2" "James Buchal"     (Just "Rep") (Just "Attorney")
          , Option "r3" "Jeffrey J Langan" (Just "L")   Nothing
          , Option "r4" "Michael Meo"      (Just "Grn") (Just "retired schoolteacher")
          , Option "r5" "David Walker"     (Just "Non") (Just "Family Nurse Practitioner")
          ]
        }
      ]
    }
  ]
