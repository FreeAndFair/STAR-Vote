data BallotStyle
data Precinct

data VoterID
data VoterStatus

data Book
data BookSticker = BookSticker
  { voter    :: VoterID
  , precinct :: Precinct -- invariant: matches the precinct in style
  , style    :: BallotStyle
  }
data VoterSignature

data Barcode a
data Digital a
type BallotCastingID = Integer -- unique; MAY be predictable
type BallotID = Integer -- unique and unpredictable
data PaperBallot = PaperBallot
  { ballot :: Ballot
  , castingID :: BallotCastingID
  , id :: BallotID -- aka serial number
  }
data Encrypted k a
data Hash a
data NIZK

data Receipt = Receipt
  { hash :: Hash Ballot
  , terminal :: Terminal
  , time :: DateTime
  }

data BBRecord
  = EncryptedSpoiled
    { hash :: Hash Ballot
    , encrypted :: Encrypted Trustees Ballot
    }
  | Vote
    { hash :: Hash Ballot
    , wellFormed :: NIZK
    }
  | DecryptedSpoiled
    { hash :: Hash Ballot
    , unencrypted :: Ballot
    }

data StatusDB -- should be instantiated once
type BallotDB = [(BallotCode, ID BallotStyle)]
data Status = Unknown | Spoiled | Cast | ProvisionallyCast
data DigitalBallot = DigitalBallot
  { bcid       :: BallotCastingID
  , ballot     :: Encrypted Trustees Ballot
  , status     :: Status
  , wellFormed :: NIZK
  , races      :: Encrypted Trustees (BallotID, [Race])
  , terminalID :: Terminal
  }
type DigitalBallotBox = [DigitalBallot]
type PaperBallotBox = [PaperBallot]
type ProvisionalBallotBox = [(VoterID, PaperBallot)]

data ControllerState = ControllerState
  { codes          :: BallotDB
  , internalChains :: Map Terminal [HashChain Internal]
  , publicChains   :: Map Terminal [HashChain Public]
  , ballots        :: DigitalBallotBox
  }
data TerminalState = TerminalState
  { initial  :: forall a. HashChain a
  , internal :: HashChain Internal
  , public   :: HashChain Public
  , key      :: PublicKey Trustees
  , styles   :: Map (ID BallotStyle) BallotStyle
  , id       :: Terminal
  }
data BallotPlus = BallotPlus
  { encrypted  :: Encrypted Trustees Ballot
  , paper      :: PaperBallot
  , receipt    :: Receipt
  , wellFormed :: NIZK
  }

-- election setup operations
initStatusDB :: [(VoterID, Precinct)] -> StatusDB
initElection :: EML -> RaceDB
generateInitialHash :: Random (forall a. HashChain a)
generateKeyPair :: [Trustee] -> Nat -> Random (PublicKey Trustees, [Shares Trustees]) -- step 1 of full crypto protocol
collude :: [Shares Trustees] -> PrivateKey Trustees

-- election definition
precinct :: BallotStyle -> Precinct

-- Voter Status database
atomicSwapVoterStatus :: VoterID -> VoterStatus -> StatusDB -> (VoterStatus, StatusDB)

-- Registration desk
register :: VoterID -> Precinct -> RaceDB -> (Barcode BallotStyle, BookSticker)
lookupPrecincts :: VoterID -> StatusDB -> NonEmpty Precinct

-- poll book updates (non-computational, just encoded here for documentation)
put :: BookSticker Book -> Book

-- backup of the voter status database
sign :: VoterID -> VoterSignature -> BookSticker -> Book -> Book

-- Controller, aka Judge's Station
generateCode :: Barcode BallotStyle -> BallotDB -> (BallotCode, BallotDB)
markAsFilledOut :: BallotCastingID -> Encrypted Trustees Ballot -> Receipt -> DigitalBallotBox -> DigitalBallotBox
-- spoiling has some funny attack: maybe somebody could ask for a code for the wrong ballot style. what gives?
spoil :: PaperBallot -> DigitalBallotBox -> BallotDB -> (BallotCode, BallotDB, DigitalBallotBox, PaperBallot)
-- TODO: gc :: Time -> BallotDB -> BallotDB


-- Voting terminal
claimBallot :: BallotCode -> State ControllerState (ID BallotStyle)
fillOutBallot :: ID BallotStyle -> UserInput -> State TerminalState BallotPlus
-- (a bit odd, but just doing it to be really clear about how this corresponds
-- with the paper's described sequence of events)
initialize :: TerminalState -> State TerminalState () -- step 2

-- offline review station
review :: PaperBallot -> Sound

-- Ballot box
cast :: PaperBallot -> DigitalBallotBox -> PaperBallotBox -> (DigitalBallotBox, PaperBallotBox)
scan :: PaperBallot -> BallotID
provisionalCast :: VoterID -> PaperBallot -> DigitalBallotBox -> ProvisionalBallotBox -> (DigitalBallotBox, ProvisionalBallotBox)

-- Bulletin board
post :: BBRecord -> [BBRecord] -> [BBRecord]
post = (:)

-- Mixer(s) implement this interface and get plugged in to the crypto bit
mix :: [BBRecord] -> ([BBRecord], proof that the input = output up to ordering)

-- audit
auditDecrypt :: [BBRecord] -> [Hash (BallotID, Race), BallotChoice]

-- crypto algorithms, used as components of various other systems
chainInternal :: HashChain Internal -> Terminal -> Event -> HashChain Internal
chainPublic   :: HashChain Public   -> Terminal -> Event -> HashChain Public
commit :: Encrypted k a -> Commitment k a
pprint :: Hash a -> String -- human readable
add :: Encrypted k Nat -> Encrypted k Nat -> Encrypted k Nat
decrypt :: PrivateKey k -> Encrypted k a -> a
wellFormed :: Ballot -> NIZK
