{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, UndecidableInstances #-}
module Application.Star.Util where

import Control.Applicative
import Control.Concurrent.STM hiding (atomically)
import Control.Monad.CatchIO
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid (AcidState, EventResult, EventState, QueryEvent, UpdateEvent, query, update)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Char
import Data.Default
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import Snap.Core hiding (method)
import Snap.Iteratee (TooManyBytesReadException)
import Snap.Http.Server (quickHttpServe)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map  as M
import qualified Data.Text as T

newtype TVarT s m a = TVarT { unTVarT :: ReaderT (TVar s) m a } deriving (Functor, Applicative, Monad)
runTVarT = runReaderT . unTVarT

instance MonadIO m => MonadState s (TVarT s m) where
  get   = TVarT (join (asks (atomically . readTVar)))
  put s = TVarT (join (asks (atomically . (`writeTVar` s))))
  state f = transaction (return . f)

class (MonadState s m, MonadIO m) => MonadTransaction s m where
  transaction  :: (s -> STM (a, s)) -> m a
  transaction_ :: (s -> STM a) -> m a

instance MonadIO m => MonadTransaction s (TVarT s m) where
  transaction f = TVarT $ do
    tvar <- ask
    atomically $ do
      old <- readTVar tvar
      (result, new) <- f old
      writeTVar tvar new
      return result

  transaction_ f = TVarT $ do
    tvar <- ask
    atomically $ readTVar tvar >>= f

atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically


-- | Run an ACID-state query in a smart enough monad
doQuery :: (MonadIO m, MonadState (AcidState (EventState event)) m, QueryEvent event)
          => event
          -> m (EventResult event)
doQuery e = do st <- get
               liftIO (query st e)

-- | Run an ACID-state update in a smart enough monad
doUpdate :: (MonadIO m, MonadState (AcidState (EventState event)) m, UpdateEvent event)
           => event
           -> m (EventResult event)
doUpdate e = do st <- get
                res <- liftIO (update st e)
                put st
                return res

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

-- TODO: don't store everything in memory
statefulErrorServeDef :: Default s => TVarT s (ExceptT Text Snap) a -> IO ()
statefulErrorServeDef m = statefulErrorServe m def

statefulErrorServe :: TVarT s (ExceptT Text Snap) a -> s -> IO ()
statefulErrorServe m s = do
  sRef <- newTVarIO s
  quickHttpServe (do
    v_ <- runExceptT (runTVarT m sRef)
    case v_ of
      Left err -> errorResponse err
      Right v  -> return ()
    )

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
reportWhere name (Right v) = return v

readURIParam, readBodyParam :: (MonadError Text m, MonadSnap m, Read a) => ByteString -> m a
readURIParam  = readParam rqQueryParams
readBodyParam = readParam rqPostParams

render :: MonadSnap m => Html -> m ()
render h = do
  modifyResponse $ setContentType "text/html"
                 . setHeader (mk "Cache-Control") "max-age=0"
  writeLBS (renderHtml h)

-- a bunch of uninteresting instances {{{
instance MonadTrans (TVarT s) where lift = TVarT . lift
instance MonadFix  m => MonadFix  (TVarT s m) where mfix f = TVarT (mfix (unTVarT . f))
instance MonadIO   m => MonadIO   (TVarT s m) where liftIO = lift . liftIO
instance MonadPlus m => MonadPlus (TVarT s m) where
  mzero = TVarT mzero
  mplus (TVarT m) (TVarT m') = TVarT (mplus m m')
instance MonadReader r m => MonadReader r (TVarT s m) where
  ask = lift ask
  local f (TVarT m) = TVarT (ReaderT (\r -> local f (runReaderT m r)))
  reader f = lift (reader f)
instance MonadError e m => MonadError e (TVarT s m) where
  throwError = lift . throwError
  catchError (TVarT m) f = TVarT $ catchError m (unTVarT . f)
instance MonadCatchIO m => MonadCatchIO (TVarT s m) where
  catch   (TVarT m) f = TVarT $ catch   m (unTVarT . f)
  block   (TVarT m)   = TVarT $ block   m
  unblock (TVarT m)   = TVarT $ unblock m
instance MonadSnap m => MonadSnap (TVarT s m) where
  liftSnap = lift . liftSnap
instance Alternative m => Alternative (TVarT s m) where
  empty = TVarT empty
  TVarT a <|> TVarT b = TVarT (a <|> b)
  some (TVarT a) = TVarT (some a)
  many (TVarT a) = TVarT (many a)

-- | Warning: this instance is somewhat contentious, for the same reason the
-- ErrorT instance is. See the MonadCatchIO-transformers documentation for
-- details.
instance MonadCatchIO m => MonadCatchIO (ExceptT e m) where
  m `catch` f = mapExceptT (\m' -> m' `catch` \e -> runExceptT $ f e) m
  block       = mapExceptT block
  unblock     = mapExceptT unblock
instance (MonadSnap m, Monoid e) => MonadSnap (ExceptT e m) where
  liftSnap = lift . liftSnap
-- }}}
