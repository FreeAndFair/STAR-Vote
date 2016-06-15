module VerifiableElGamal where

import Data.Ix
import Data.Monoid

class Monoid g => Group g where invert :: g -> g
class Group g => FiniteGroup g where order :: proxy g -> Integer

data PublicKey g = PublicKey
	{ generator :: g
	, targetPow :: g
	} deriving (Eq, Ord, Read, Show)

data PrivateKey g = PrivateKey
	{ targetLog :: Integer
	, publicKey :: PublicKey g
	} deriving (Eq, Ord, Read, Show)

data Encryption g = Encryption
	{ sourcePow :: g
	, prod      :: g
	} deriving (Eq, Ord, Read, Show)

data Evidence g = Evidence
	{ commitment :: g
	, response   :: Integer
	} deriving (Eq, Ord, Read, Show)

pow :: Group g => g -> Integer -> g
pow g n | n < 0 = invert (pow g (negate n))
pow g 0 = mempty
pow g 1 = g
pow g n = case n `quotRem` 2 of
	(q, r) -> let halfsies = pow g q in pow g r <> halfsies <> halfsies

encrypt :: FiniteGroup g => PublicKey g -> g -> Integer -> Encryption g
encrypt pub message randomness | inRange (1, order pub - 1) randomness = Encryption
	{ sourcePow = pow (generator pub) randomness
	, prod      = message <> pow (targetPow pub) randomness
	}

type ZKHash g = Integer -> g -> Integer

zk :: FiniteGroup g => ZKHash g -> PublicKey g -> Integer -> [Integer] -> [Evidence g]
zk hash pub encryptionRandomness commitmentRandomness
	|  inRange (1, order pub - 1) encryptionRandomness
	&& all (inRange (0, order pub - 1)) commitmentRandomness
	&& all (inRange (0, order pub - 1)) hashes
	= zipWith3 prove commitmentRandomness commitments hashes where
	hashes = drop 1 $ scanl hash 0 commitments
	commitments = map (pow (targetPow pub)) commitmentRandomness
	prove randomness power challenge = Evidence
		{ commitment = power
		, response   = (randomness + challenge * encryptionRandomness) `mod` order pub
		}

verify :: (Eq g, FiniteGroup g) => ZKHash g -> PublicKey g -> g -> Encryption g -> [Evidence g] -> Bool
verify hash pub plaintext ciphertext evidence
	| all (inRange (0, order pub - 1)) hashes
	= and (zipWith3 verifySingle commitments hashes responses) where
	hashes      = drop 1 $ scanl hash 0 commitments
	commitments = map commitment evidence
	responses   = map response   evidence
	power       = invert plaintext <> prod ciphertext
	base        = targetPow pub
	verifySingle commitment hash response = pow base response == commitment <> pow power hash

decrypt :: Group g => PrivateKey g -> Encryption g -> g
decrypt priv enc = prod enc <> pow (sourcePow enc) (-targetLog priv)

-- this is not a secure group or a good hash! just for testing purposes
instance Monoid Integer where
	mempty = 0
	mappend x y = (x + y) `mod` 7
instance Group Integer where
	invert x = negate x `mod` 7
instance FiniteGroup Integer where
	order _ = 7

pubKey :: PublicKey Integer
privKey :: PrivateKey Integer
hash :: ZKHash Integer
pubKey = PublicKey { generator = 2, targetPow = pow 2 4 }
privKey = PrivateKey { targetLog = 4, publicKey = pubKey }
hash n g = (n * g + 1) `mod` 7

pt :: Integer
encRand :: Integer
commitRand :: [Integer]
ct :: Encryption Integer
ev :: [Evidence Integer]
pt' :: Integer
valid :: Bool
pt = 3
encRand = 2
commitRand = [0..6]
ct = encrypt pubKey pt encRand
ev = zk hash pubKey encRand commitRand
pt' = decrypt privKey ct
valid = verify hash pubKey pt' ct ev
