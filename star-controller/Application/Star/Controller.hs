{-# LANGUAGE FlexibleContexts, OverloadedStrings, Rank2Types, TemplateHaskell #-}
module Application.Star.Controller where

{-
generateCode :: Barcode BallotStyle -> BallotDB -> (BallotCode, BallotDB)
markAsFilledOut :: BallotCastingID -> Encrypted Trustees Ballot -> Receipt -> DigitalBallotBox -> DigitalBallotBox
spoil :: PaperBallot -> DigitalBallotBox -> BallotDB -> (BallotCode, BallotDB, DigitalBallotBox, PaperBallot)
claimBallot :: BallotCode -> State ControllerState (ID BallotStyle)
cast :: PaperBallot -> DigitalBallotBox -> PaperBallotBox -> (DigitalBallotBox, PaperBallotBox)
provisionalCast :: VoterID -> PaperBallot -> DigitalBallotBox -> ProvisionalBallotBox -> (DigitalBallotBox, ProvisionalBallotBox)
-}

import Application.Star.Ballot
import Application.Star.BallotStyle
import Application.Star.HashChain
import Application.Star.ID
import Application.Star.SerializableBS
import Application.Star.Util
import Application.Star.CommonImports hiding (method)
import Control.Arrow
import Control.Lens
import Data.Char
import System.Random

import qualified Control.Concurrent.STM as STM
import qualified Data.Map  as M
import qualified Data.Text as T

type BallotDB = Map BallotCode (ID BallotStyle)
type TMap k v = Map k (TVar v)
data ControllerState = ControllerState
	{ _seed :: StdGen
	, _outstandingBallots :: BallotDB
	-- ballotBox invariant: the bcid in the EncryptedRecord matches the key it's filed under in the Map
	, _ballotBox :: TMap BallotCastingId (BallotStatus, EncryptedRecord)
	}

makeLenses ''ControllerState

main :: IO ()
main = do
	seed <- getStdGen
	statefulErrorServe controller $ ControllerState seed def def

controller :: (MonadError Text m, MonadTransaction ControllerState m, MonadSnap m) => m ()
controller = route $
	[ ("generateCode", do
		method POST
		styleID <- readBodyParam "style"
		code    <- generateCode styleID
		-- TODO: broadcast (styleID, code) to voting terminals
		writeShow code
	  )
	, ("fillOut", do
		method POST
		ballot <- readJSONBody
		transaction (fillOut ballot)
	  )
	, ("cast", do
		method POST
		castingID <- BallotCastingId <$> readBodyParam "bcid"
		cast castingID
	  )
	]

-- generateCode generates a fresh code by first trying a few random codes; if
-- that doesn't pan out, it searches all possible codes for any it could use
-- {{{
generateCode :: (MonadError Text m, MonadState ControllerState m) => ID BallotStyle -> m BallotCode
generateCode style = freshRandom retries where
	retries = 20 -- magic number picked out of a hat
	freshRandom n
		| n > 0 = do
			c <- randomCode
			success <- state' outstandingBallots (registerCode c style)
			if success then return c else freshRandom (n-1)
		| otherwise = freshSearch
	freshSearch = minimalCode style

minimalCode :: (MonadError Text m, MonadState ControllerState m) => ID BallotStyle -> m BallotCode
minimalCode style = join $ state' outstandingBallots go where
	go db = case M.minView (M.difference allCodes db) of
		Just (code, _) -> (return code, M.insert code style db)
		Nothing        -> (throwError "all ballot codes in use", db)

allCodes :: Map BallotCode BallotCode
allCodes = M.fromList [(k, k) | k <- [minBound..maxBound]]

registerCode :: BallotCode -> ID BallotStyle -> BallotDB -> (Bool, BallotDB)
registerCode code style db
	| not (code `M.member` db) = (True, M.insert code style db)
	| otherwise = (False, db)

randomCode :: MonadState ControllerState m => m BallotCode
randomCode = state' seed random
-- }}}

fillOut ballot s = do
	p <- STM.newTVar (Unknown, ballot)
	-- TODO: is it okay to always insert? do we need to check that it
	-- isn't there first or something?
	return ((), set ballotBox (M.insert (_bcid ballot) p (_ballotBox s)) s)

cast bcid = join . transaction_ $ \s -> do
	case M.lookup bcid (_ballotBox s) of
		Just p -> do
			(status, record) <- STM.readTVar p
			case status of
				Unknown -> STM.writeTVar p (Cast, record) >> return (return ())
				_ -> return (throwError $ T.pack (show bcid) <> " was already " <> T.pack (map toLower (show status)))
		_ -> return (throwError $ "Unknown " <> T.pack (show bcid))

state' :: MonadState s m => Lens s s t t -> (t -> (a, t)) -> m a
state' lens f = state (\s -> second (flip (set lens) s) (f (view lens s)))
