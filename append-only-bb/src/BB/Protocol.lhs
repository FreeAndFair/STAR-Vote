> {-# LANGUAGE MultiWayIf, PatternGuards, OverloadedStrings #-}

> {-| This is a pure implementation of the communications protocol for
> posting to and reading from the BB. This is a literate Haskell file
> please read the source.
> -}

> module BB.Protocol where

> import BB.Time

> import Control.Applicative

> import Crypto.Hash
> import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
> import Crypto.PubKey.ECC.Generate
> import Crypto.PubKey.HashDescr
> import Crypto.Random
> import Crypto.Types.PubKey.ECC

> import Data.Byteable

> import Data.ByteString (ByteString)
> import qualified Data.ByteString as BS
> import qualified Data.ByteString.Lazy as BSL
> import qualified Data.ByteString.Lazy.UTF8 as UTF8

> import Data.Foldable (foldrM, foldlM)

> import Data.List (intercalate)

> import Data.Monoid

> import Data.Time (UTCTime, NominalDiffTime, addUTCTime)

> import qualified Data.Text as T
>
> sha256 :: Byteable a => a -> Digest SHA256
> sha256 = hash . toBytes

For purposes of the communication protocol, all hashes are ByteStrings

> type Hash = ByteString


We need an intial sequence hash to use when the board is
empty. Following the paper, call it "0".

> initialHash :: Hash
> initialHash = "0"

> instance (Byteable a, Byteable b) => Byteable (a, b) where
>   toBytes (a, b) = toBytes a <> toBytes b

> instance (Byteable a, Byteable b, Byteable c) => Byteable (a, b, c) where
>   toBytes (a, b, c) = toBytes a <> toBytes b <> toBytes c

> instance (Byteable a, Byteable b, Byteable c, Byteable d) => Byteable (a, b, c, d) where
>   toBytes (a, b, c, d) = toBytes a <> toBytes b <> toBytes c <> toBytes d

Signatures and verifications

> data Signed a = Signed { message :: a, signature :: ECDSA.Signature }
>   deriving Show
>
> sign :: (CPRG g, Byteable a) => g -> ECDSA.PrivateKey -> a -> (Signed a, g)
> sign g priv x = let (signature, g') =
>                       ECDSA.sign g priv (hashFunction hashDescrSHA256) $ toBytes x
>                 in (Signed x signature, g')
>
> verify :: Byteable a => ECDSA.PublicKey -> Signed a -> Bool
> verify public (Signed x sig) = ECDSA.verify (hashFunction hashDescrSHA256) public sig (toBytes x)


Necessary to sign signatures:

> instance Byteable ECDSA.Signature where
>   toBytes (ECDSA.Signature r s) = bytes r <> bytes "||" <> bytes s
>     where bytes :: Show a => a -> ByteString
>           bytes = mconcat . BSL.toChunks . UTF8.fromString . show
>
> instance (Byteable a) => Byteable (Signed a) where
>   toBytes (Signed x s) = toBytes x <> toBytes s

First, reading from the BB

The BB consists of a sequence of zero or more posts. Each post contains:

 1. A message
 2. The time at which the message was written
 3. The name of the author of the message
 4. A sequence hash, hashing the message, timestamp, author, and
    previous hash
 5. The writer's signature of the sequence hash
 6. The board's signature of the writer's signature and the time at
    which the board signed the message

> data Post' msg writer =
>   Post' { postMessage' :: msg
>         , postWrittenTime' :: UTCTime
>         , postWriter' :: writer
>         , postSequenceHash' :: Hash
>         , postWriterSig' :: Signed Hash
>         , postBoardSig' :: Signed (Signed Hash, UTCTime)
>         }

However, some of these fields are computable from others:

> data Post msg writer =
>   Post { postMessage :: msg
>        , postWrittenTime :: UTCTime
>        , postWriter :: writer
>        , postBoardSig :: Signed (Signed Hash, UTCTime)
>        }
>
> postWriterSig :: Post msg writer -> Signed Hash
> postWriterSig (Post { postBoardSig = Signed (sh, t) _ }) = sh
>
> postSequenceHash :: Post msg writer -> Hash
> postSequenceHash p = let Signed h _ = postWriterSig p
>                      in h

We can also extract the board's signature time:

> postBoardTime :: Post msg writer -> UTCTime
> postBoardTime p = let (Signed (_, ti') _) = postBoardSig p
>                   in ti'

A sequence of these messages is a history:

> type History msg writer = [Post msg writer]

A _consistent_ history is one which satisfies the following for each post:

 1. Each hash is correctly computed (ie it's a hash of the message,
    timestamp, writer ID, and previous hash), except the first message
    has a constant sequence hash 0

 2. The writer's signature of the sequence hash is valid

 3. The board's signature of the writer's signature and receipt time
    is valid

 4. The writing time is less than or equal to the board's reciept time, and
    their difference is at most epsilon

This implementation, upon success, returns the first sequence hash

First, we check if a single post is consistent, given a previous hash

> consistentP :: (Eq writer, Byteable msg, Byteable writer, Show msg, Show writer)
>             => NominalDiffTime
>             -> (writer -> Maybe ECDSA.PublicKey)
>             -> ECDSA.PublicKey
>             -> Post msg writer
>             -> Hash                 -- ^ the hash for the previous post
>             -> Either String Hash
> consistentP epsilon keys boardKey p h0 =
>   case keys w of
>     Nothing -> Left $ "Couldn't find public key for writer " ++ show w
>     Just wk ->
>       if | toBytes (sha256 (m, ti, w, h0)) /= postSequenceHash p ->
>            Left $ "Mismatching hashes: " ++
>                   "I hashed " ++ intercalate "," [show m, show ti, show w, show h0] ++
>                   " and got " ++
>                   show (toBytes (sha256 (m, ti, w, h0))) ++
>                   " and " ++ show (postSequenceHash p)
>          | not $ verify wk sw ->
>            Left "Mismatching writer signature"
>          | not $ verify boardKey sb ->
>            Left "Mismatching board signature"
>          | not $ ti <= ti' ->
>            Left "The board's verfication time was before the writing time"
>          | not $ ti' < addUTCTime epsilon ti ->
>            Left "The board took too long to verify receipt"
>          | otherwise -> Right $ postSequenceHash p
>   where m   = postMessage p
>         ti  = postWrittenTime p
>         w   = postWriter p
>         h   = postSequenceHash p
>         sw  = postWriterSig p
>         sb  = postBoardSig p
>         ti' = postBoardTime p

> consistent :: (Eq writer, Byteable msg, Byteable writer, Show writer, Show msg)
>            => NominalDiffTime
>            -> (writer -> Maybe ECDSA.PublicKey)
>            -> ECDSA.PublicKey
>            -> History msg writer
>            -> Hash                 -- ^ the hash for the empty history (0 in the paper)
>            -> Either String Hash
> consistent epsilon keys boardKey =
>   flip . foldrM $ consistentP epsilon keys boardKey



When the board is read, it sends the message history along with the
signed latest sequence hash and the current time.

This is Message 1 under "Reading" in the paper.

> readBoard :: (CPRG g, Byteable msg, Byteable writer)
>           => g
>           -> History msg writer  -- ^ The history to be read
>           -> ECDSA.PrivateKey    -- ^ The BB's private key
>           -> Hash                -- ^ The hash for an empty history (0 in the paper)
>           -> UTCTime             -- ^ Now
>           -> ((History msg writer, Signed (Hash, UTCTime)), g)
> readBoard g hist priv h0 now = let (s, g') = sign g priv (h, now)
>                                in ((hist, s), g')
>   where h | []    <- hist = h0
>           | (p:_) <- hist = postSequenceHash p

To verify this message, the writer must check:
 1. The signature matches the board's public key
 2. The signed sequence hash matches the most recent one in the history
 3. The time is sufficiently recent (ie within some factor epsilon of the current time)
 4. The history is consistent

Point 3 is not explicit in the paper, but it doesn't make sense without it.

> checkRead :: (Byteable msg, Byteable writer, Eq writer, Show writer, Show msg)
>           => (writer -> Maybe ECDSA.PublicKey) -- ^ Public keys for message authors
>           -> NominalDiffTime                   -- ^ Time tolerance
>           -> NominalDiffTime                   -- ^ Epsilon for checking BB
>           -> UTCTime                           -- ^ Now
>           -> ECDSA.PublicKey                   -- ^ The board's public key
>           -> Hash                              -- ^ The initial sequence hash
>           -> (History msg writer, Signed (Hash, UTCTime))
>           -> Either String ()
> checkRead keys tolerance epsilon now boardpub h0 (hist, sig@(Signed (h, tb) s)) =
>   if | not $ verify boardpub sig     -> Left "The signature doesn't match"  -- (1)
>      | [] <- hist , h0 /= h          -> Left "Mismatched sequence hash for empty history" -- (2)
>      | (p:_) <- hist,
>        postSequenceHash p /= h       -> Left "Mismatched sequence hash for history" -- (2)
>      | tb > now                      -> Left "The board's time is greater than the current time - check clocks" -- (3)
>      | addUTCTime tolerance tb < now -> Left "Too much time has passed since the board signed the history"      -- (3)
>      | otherwise                     -> fmap (const ()) $ consistent epsilon keys boardpub hist h0 -- (4)


Writing to the board

First step

The first step of the protocol is that the board sends the potential
poster a signed copy of the latest hash plus it's current
timestamp. The writer can use this later to prove what the state of
the board was when it posted.

> data CurrentHash =
>   CurrentHash (Signed (Hash, UTCTime))

The writer then checks the following:
 1. The signature matches the board's public key
 2. The timestamp is not more than epsilon old

Also, there's an extra check not in the paper: it checks that the
board's timestamp is not in the future.

> checkCurrentHash :: ECDSA.PublicKey -- ^ the board's public key
>                  -> UTCTime         -- ^ the timestamp now
>                  -> NominalDiffTime -- ^ epsilon
>                  -> CurrentHash     -- ^ A current timestamp and hash signature
>                  -> Either String ()
> checkCurrentHash boardpub now epsilon (CurrentHash signed@(Signed { message = (h, t) })) =
>   if | not $ verify boardpub signed -> Left "Signature didn't match"
>      | t > now                      -> Left "The board claimed to be in the future - sync clocks?"
>      | addUTCTime epsilon t < now   -> Left "Delay was too long"
>      | otherwise                    -> Right ()


Second step

The writer sends the message to the board. Accompanying this is:
 1. A timestamp giving the time of writing
 2. The writer's name
 3. A new hash for the chain, which hashes:
  - the message
  - the time of writing
  - the name of the writer
  - the previous end-of-chain hash
 4. A signature for the hash proving that the writer intends to append this message

> data NewMessage msg writer =
>   NewMessage { newMessageText :: msg
>              , newMessageWritingTime :: UTCTime
>              , newMessageWriter :: writer
>              , newMessageSignature :: Signed Hash
>              }
>  deriving Show


> prepareMessage :: (CPRG g, Byteable msg, Byteable writer)
>                => g                -- ^ Random state
>                -> ECDSA.PrivateKey -- ^ The writer's private key
>                -> msg              -- ^ the message
>                -> UTCTime          -- ^ the time of writing
>                -> writer           -- ^ the writer's name
>                -> Hash             -- ^ the previous end-of-chain hash
>                -> (NewMessage msg writer, g)
> prepareMessage g k message tw w h =
>   let h'           = toBytes $ sha256 (message, tw, w, h)
>       (signed, g') = sign g k h'
>   in (NewMessage message tw w signed, g')

When the board recieves this message, it needs to check the following:
 1. The signature is valid according to the writer's public key
 2. The hash is actually a hash of the message, the timestamp, and the writer

> checkMessage :: (Byteable msg, Byteable writer)
>              => Hash                                -- ^ The current state hash
>              -> ECDSA.PublicKey                     -- ^ The writer's public key
>              -> NewMessage msg writer               -- ^ The new post
>              -> Either String ()
> checkMessage h0 k (NewMessage message tw w sh@(Signed h sig)) =
>   if | not $ verify k sh -> Left ("The signature doesn't match")
>      | h /= myHash       -> Left ("The hash doesn't match")
>      | otherwise         -> Right ()
>   where myHash = toBytes $ sha256 (message, tw, w, h0)


Step 3

The board confirms its acceptance of the message. This is done by
sending the writer back their original signature along with the
board's time of acceptance, both signed by the board's private key.


> acceptedMessage :: CPRG g
>                 => g
>                 -> ECDSA.PrivateKey -- ^ The board's private key
>                 -> Signed Hash      -- ^ The writer's new message hash
>                 -> UTCTime          -- ^ The time at which the board accepted the message
>                 -> (Signed (Signed Hash, UTCTime), g)
> acceptedMessage g priv sh tb = sign g priv (sh, tb)


The writer then checks two things:
 1. The signature is valid
 2. The time that is signed is within epsilon of the time that the message was written

Additional verification:
 - The board time is not before the writing time

> checkAcceptedMessage :: ECDSA.PublicKey -- ^ The board's public key
>                      -> UTCTime         -- ^ The time at which the writer wrote the message
>                      -> NominalDiffTime -- ^ Epsilon
>                      -> Signed (Signed Hash, UTCTime) -- ^ The acceptance message from the board
>                      -> Either String ()
> checkAcceptedMessage boardKey tw epsilon accept@(Signed { message = (_, tb) }) =
>   if | not $ verify boardKey accept -> Left "The board signature didn't match"
>      | tw > tb                      -> Left "The board posting time is before the writing time"
>      | addUTCTime epsilon tw < tb   -> Left "The board time is too late"
>      | otherwise                    -> Right ()
