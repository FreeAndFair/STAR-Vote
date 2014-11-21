{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, UndecidableInstances #-}
module Application.Star.Util where

import Control.Applicative
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
import qualified Data.Map  as M
import qualified Data.Text as T

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
reportWhere name (Right v) = return v

readURIParam, readBodyParam :: (MonadError Text m, MonadSnap m, Read a) => ByteString -> m a
readURIParam  = readParam rqQueryParams
readBodyParam = readParam rqPostParams

render :: MonadSnap m => Html -> m ()
render h = do
  modifyResponse $ setContentType "text/html"
                 . setHeader (mk "Cache-Control") "max-age=0"
  writeLBS (renderHtml h)

