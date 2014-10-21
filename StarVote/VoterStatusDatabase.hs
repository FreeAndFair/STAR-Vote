{-# LANGUAGE OverloadedStrings #-}
module VoterStatusDatabase where

import Application.Star.ID
import Application.Star.Precinct
import Application.Star.Voter
import Control.Arrow
import Control.Applicative
import Control.Monad.Reader
import Data.Default
import Data.List
import Data.Map  (Map)
import Data.Traversable
import Numeric
import Snap (method)
import Util hiding (method) -- TODO: transition this file to Util-style error handling

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.Map        as M
import qualified Data.Text       as T

-- The interface consists of three possible requests. Below we give the HTTP
-- method and path that should be used, together with any parameters that it
-- takes marked by either "URI" for parameters that should go in the URI or
-- "Body" for parameters that should go in the request body. The parameters are
-- decoded using UTF8 then parsed using read. The Read and Show instance for
-- (ID x) is not the derived one; it is the Integer instance.
-- GET   lookup           :: { voter :: URI  (ID Voter)                        } -> [ID Precinct]
-- POST  initialize       :: { db    :: Body [(ID Voter, ID Precinct)]         } -> {- produces the empty string as output -}
-- PATCH atomicSwapStatus :: { voter :: Body (ID Voter), status :: Body Status } -> Status

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
			case M.lookup voter db of
				Nothing -> return []
				Just p  -> snd <$> readTVar p
		writeShow v
	  )
	, ("initialize", method POST . useBodyParam "db" $ \dbData -> do
		db <- atomically $ buildStatusDB dbData
		atomically (writeTVar dbRef db)
	  )
	, ("atomicSwapStatus", method PATCH
	                     . useBodyParam "voter"  $ \voter  ->
	                       useBodyParam "status" $ \status -> do
		cont <- atomically $ do
			db <- readTVar dbRef
			case M.lookup voter db of
				Just p -> do
					(oldStatus, precincts) <- readTVar p
					writeTVar p (status, precincts)
					return (writeShow oldStatus)
				Nothing -> return (errorResponse "illegal voter")
		cont
	  )
	]

-- TODO: transition these to the Util.read*Param style
useURIParam, useBodyParam :: Read a => ByteString -> (a -> Snap ()) -> Snap ()
useURIParam  = useParam rqQueryParams
useBodyParam = useParam rqPostParams

useParam :: Read a => (Request -> Params) -> ByteString -> (a -> Snap ()) -> Snap ()
useParam extractParams name f = runExceptT (readParam extractParams name) >>= either errorResponse f

buildStatusDB :: [(ID Voter, ID Precinct)] -> STM StatusDB
buildStatusDB = traverse newTVar . M.fromListWith combine . map inject where
	combine (s1, ps1) (s2, ps2) = (s1, ps1 <> ps2)
	inject (voter, precinct) = (voter, (Hasn't, [precinct]))
