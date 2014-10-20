module Types where

data ID a = ID { getID :: Integer } deriving (Eq, Ord, Read, Show)

instance Num (ID a) where
	ID a + ID b = ID (a + b)
	ID a * ID b = ID (a * b)
	ID a - ID b = ID (a - b)
	negate (ID a) = ID (negate a)
	abs    (ID a) = ID (abs    a)
	signum (ID a) = ID (signum a)
	fromInteger   = ID

data Voter
data Precinct
data VoterStatus = Voted | Hasn't deriving (Bounded, Enum, Eq, Ord, Read, Show)

data BallotStyle
type BallotCode = Int -- 00000 to 99999
type BallotDB = [(BallotCode, ID BallotStyle)]
