{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Controller where

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
import Application.Star.ID
import Control.Arrow
import Control.Monad.CatchIO
import System.Random
import Util

type BallotDB = [(BallotCode, ID BallotStyle)]
data ControllerState = ControllerState
	{ _outstandingBallots :: BallotDB
	, _seed :: StdGen
	} deriving (Read, Show)

-- TODO: lens
getOutstandingBallots = _outstandingBallots
putOutstandingBallots new s = s { _outstandingBallots = new }
getSeed = _seed
putSeed new s = s { _seed = new }

main :: IO ()
main = do
	seed <- getStdGen
	statefulErrorServe controller $ ControllerState def seed

controller :: (MonadError Text m, MonadState ControllerState m, MonadSnap m) => m ()
controller = route $
	[ ("generateCode", do
		method POST
		styleID <- readBodyParam "style"
		code    <- generateCode styleID
		writeShow code
	  )
	]

-- generateCode generates a fresh code by first trying a few random codes; if
-- that doesn't pan out, it searches all possible codes for any it could use
generateCode :: (MonadError Text m, MonadState ControllerState m) => ID BallotStyle -> m BallotCode
generateCode style = freshRandom retries where
	retries = 20 -- magic number picked out of a hat
	freshRandom n
		| n > 0 = do
			c <- randomCode
			success <- state' getOutstandingBallots putOutstandingBallots (registerCode c style)
			if success then return c else freshRandom (n-1)
		| otherwise = freshSearch
	freshSearch = throwError "couldn't find a fresh code" -- TODO: try harder

registerCode :: BallotCode -> ID BallotStyle -> BallotDB -> (Bool, BallotDB)
registerCode code style db
	| code `notElem` map fst db = (True, (code, style):db)
	| otherwise                 = (False, db)

randomCode :: MonadState ControllerState m => m BallotCode
randomCode = state' getSeed putSeed random

state' :: MonadState s m => (s -> s') -> (s' -> s -> s) -> (s' -> (a, s')) -> m a
state' get put f = state (\s -> second (flip put s) (f (get s)))
