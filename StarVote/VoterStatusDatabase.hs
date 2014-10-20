{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module VoterStatusDatabase where

import Control.Arrow
import Control.Concurrent.STM hiding (atomically)
import Control.Applicative
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Char
import Data.Default
import Data.List
import Data.Map  (Map)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import Data.Traversable
import Numeric
import Snap
import Types

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.Map        as M
import qualified Data.Text       as T

-- The interface consists of three possible requests. Below we give the HTTP
-- method and path that should be used, together with any parameters that it
-- takes marked by either "URI" for parameters that should go in the URI or
-- "Body" for parameters that should go in the request body. The parameters are
-- decoded using UTF8 then parsed using read. In inputs, where we write (ID x),
-- we use the Integer instance of read and wrap it with an ID constructor
-- ourselves.
-- GET   lookup           :: { voter :: URI  (ID Voter)                     } -> [ID Precinct]
-- POST  initialize       :: { db    :: Body [(ID Voter, ID Precinct)]      } -> {- produces the empty string as output -}
-- PATCH atomicSwapStatus :: { voter :: Body Integer, status :: Body Status } -> Status

type TMap  k v = Map k (TVar v)
type StatusDB  = TMap (ID Voter) (VoterStatus, [ID Precinct])

main :: IO ()
main = do
	dbRef <- atomically $ newTVar def -- for now, just store in memory
	quickHttpServe (voterStatusDB dbRef)

voterStatusDB :: TVar StatusDB -> Snap ()
voterStatusDB dbRef = route
	[ ("lookup", method GET . useURIParam "voter" $ \voter -> do
		v <- atomically $ do
			db <- readTVar dbRef
			case M.lookup (ID voter) db of
				Nothing -> return []
				Just p  -> snd <$> readTVar p
		writeShow v
	  )
	, ("initialize", method POST . useBodyParam "db" $ \dbData -> do
		db <- atomically . buildStatusDB . map (ID *** ID) $ dbData
		atomically (writeTVar dbRef db)
	  )
	, ("atomicSwapStatus", method PATCH
	                     . useBodyParam "voter"  $ \voter  ->
	                       useBodyParam "status" $ \status -> do
		cont <- atomically $ do
			db <- readTVar dbRef
			case M.lookup (ID voter) db of
				Just p -> do
					(oldStatus, precincts) <- readTVar p
					writeTVar p (status, precincts)
					return (writeShow oldStatus)
				Nothing -> return (errorResponse "illegal voter")
		cont
	  )
	]

errorResponse :: Text -> Snap ()
errorResponse err = modifyResponse (setResponseCode 400) >> writeText err

useURIParam, useBodyParam :: Read a => ByteString -> (a -> Snap ()) -> Snap ()
useURIParam  = useParam rqQueryParams
useBodyParam = useParam rqPostParams

useParam :: Read a => (Request -> Params) -> ByteString -> (a -> Snap ()) -> Snap ()
useParam extractParams name f = readParam extractParams name >>= either errorResponse f

readParam :: Read a => (Request -> Params) -> ByteString -> Snap (Either Text a)
readParam extractParams name = do
	params <- extractParams <$> getRequest
	return . reportWhere $ case M.lookup name params of
		Just (bs:_) -> case decodeUtf8' bs of
			Right t -> case reads (T.unpack t) of
				(v, rest):_ | all isSpace rest -> Right v
				_ -> Left "unparseable"
			_ -> Left "badly encoded"
		_ -> Left "missing"
	where
	reportWhere (Left  s) = Left (s <> " argument in parameter " <> T.pack (show name))
	reportWhere (Right v) = Right v

buildStatusDB :: [(ID Voter, ID Precinct)] -> STM StatusDB
buildStatusDB = traverse newTVar . M.fromListWith combine . map inject where
	combine (s1, ps1) (s2, ps2) = (s1, ps1 <> ps2)
	inject (voter, precinct) = (voter, (Hasn't, [precinct]))

writeShow :: Show a => a -> Snap ()
writeShow = writeText . T.pack . show

atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically
