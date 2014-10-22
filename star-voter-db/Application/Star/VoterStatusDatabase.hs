{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Application.Star.VoterStatusDatabase where

import Application.Star.CommonImports hiding (method)
import Application.Star.ID
import Application.Star.Precinct
import Application.Star.Util
import Application.Star.Voter
import Control.Arrow
import Data.List
import Data.Traversable
import Numeric

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
main = statefulErrorServeDef voterStatusDB

voterStatusDB :: TVarT StatusDB (ExceptT Text Snap) ()
voterStatusDB = route
	[ ("lookup", do
		method GET
		voter <- readURIParam "voter"
		v <- tvarT_ $ \db -> case M.lookup voter db of
			Nothing -> return []
			Just p  -> snd <$> STM.readTVar p
		writeShow v
	  )
	, ("initialize", do
		method POST
		dbData <- readBodyParam "db"
		db <- atomically $ buildStatusDB dbData
		put db
	  )
	, ("atomicSwapStatus", do
		method PATCH
		voter  <- readBodyParam "voter"
		status <- readBodyParam "status"
		cont   <- tvarT_ $ \db -> case M.lookup voter db of
			Just p -> do
				(oldStatus, precincts) <- STM.readTVar p
				STM.writeTVar p (status, precincts)
				return (writeShow oldStatus)
			Nothing -> return (errorResponse "illegal voter")
		cont
	  )
	]

buildStatusDB :: [(ID Voter, ID Precinct)] -> STM StatusDB
buildStatusDB = traverse STM.newTVar . M.fromListWith combine . map inject where
	combine (s1, ps1) (s2, ps2) = (s1, ps1 <> ps2)
	inject (voter, precinct) = (voter, (Hasn't, [precinct]))
