-- extensions and imports {{{
{-# LANGUAGE
      AutoDeriveTypeable,
      FlexibleContexts,
      FlexibleInstances,
      OverloadedStrings,
      PackageImports,
      StandaloneDeriving,
      TemplateHaskell,
      TypeFamilies,
      TypeSynonymInstances
  #-}
module Main where

import Application.Star.CommonImports
import Application.Star.Util hiding (method)
import Control.Exception
import Control.Monad.CryptoRandom
import Crypto.PubKey.ECC.ECDSA
import "crypto-random" Crypto.Random
import Crypto.Random.DRBG
import Crypto.Hash.CryptoAPI
import Crypto.Types.PubKey.ECC
import Data.Acid
import Data.Aeson
import Data.Aeson.TH
import Data.Array (assocs)
import Data.Byteable
import Data.ByteString.Lazy (toChunks)
import Data.SafeCopy
import Data.String
import Data.Text (Text)
import Data.Time.Clock
import Data.Typeable
import Paths_star_crypto
import StarVote.Crypto.Groups
import StarVote.Crypto.ThresholdElGamal
import StarVote.Crypto.Types
import System.IO
import Text.Blaze.Html4.Strict
import Text.Blaze.Html4.Strict.Attributes hiding (method)

import qualified BB.DB as BB
import qualified BB.Protocol as BB
import qualified Data.Acid.Advanced as Acid
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Text as Text
import qualified Network.HTTP as HTTP
import qualified Text.Blaze.Html4.Strict as Tag
import qualified Text.Blaze.Html4.Strict.Attributes as Attr
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.Random.DRBG.HMAC as HMAC
-- }}}
-- boring stuff that must come early due to TH staging restrictions {{{
newtype PossibleKey = PossibleKey (Maybe PublicKey)
instance Default PossibleKey where def = PossibleKey def

keyUnchanged :: PublicKey -> Update PossibleKey (Either Text ())
keyUnchanged new = runExceptT $ do
  PossibleKey mold <- get
  case mold of
    Nothing  -> put . PossibleKey . Just $ new
    Just old -> unless (old == new) (throwError "Yikes, the bulletin board key has changed out from under us! Something is very wrong.")

makeAcidic ''PossibleKey ['keyUnchanged]
-- }}}

main :: IO ()
main = do
  -- TODO: It would be nice if there were a way to only generate a seed when
  -- loading from the acid-state database failed, since this probably uses up
  -- real entropy.
  -- see https://github.com/acid-state/acid-state/issues/47
  seed      <- newGenIO :: IO HmacDRBG
  stateFile <- getDataFileName "key-generation"
  bbKeyFile <- getDataFileName "bb-public-key"
  state     <- openLocalStateFrom stateFile seed
  bbKey     <- openLocalStateFrom bbKeyFile def
  contact   <- readContactInfo state
  flip statefulErrorServe state . flip runReaderT contact . route $
    [ methodName GET  "initialize.html" quorumConfiguration
    , methodName POST "initialize.html" (generateShares bbKey)
    , methodName GET  "register.html"   registerForm
    , methodName POST "register.html"   register
    , methodName POST "republish.html"  (republish bbKey)
    ]

readContactInfo :: AcidState HmacDRBG -> IO BBContactInfo
readContactInfo state
  = (getDataFileName "bb-contact-info" >>= readFile >>= readIO)
  `catch` initContactInfo state

initContactInfo :: AcidState HmacDRBG -> IOException -> IO BBContactInfo
initContactInfo state _ = do
  contactFile <- getDataFileName "bb-contact-info"
  hPutStrLn stderr $ "Missing or corrupt contact information for the bulletin board. Writing a default configuration to <" <> contactFile <> ">."
  -- TODO: this curve was chosen arbitrarily, perhaps some more thought should
  -- go into this
  (pub, priv) <- update state (BuildKeyPairECC (getCurveByName SEC_p112r1))
  let result = BBContactInfo "localhost:8000" (BB.Author "Election Trustees" pub) priv
  writeFile contactFile (show result)
  return result

quorumConfiguration = page "Quorum Configuration" $ do
  form ! Attr.method "POST" $ do
    question "trustee_count" "1" "How many trustees are there?"
    question "threshold"     "1" "How many trustees should be required when finalizing the election?"
    Tag.div (input ! type_ "submit" ! value "generate key shares")

generateShares bbKey = do
  n <- readBodyParam "trustee_count"
  t <- readBodyParam "threshold"
  when (n < 1) (throwError "Trustee count must be positive.")
  when (n < t) (throwError "You can't demand more trustees than there are!")
  let params = TEGParams
        { tegOrder     = modp2048Prime
        , tegGenerator = modp2048Generator
        , tegTrustees  = n
        , tegThreshold = t
        }
  (TEGPublicKey _ public, TEGPrivateKey _ private) <- errorUpdate (BuildKeyPairTEG params)
  shares <- errorUpdate (BuildShares params private)
  publishKey bbKey public shares

republish bbKey = do
  public <- readBodyParam "public"
  shares <- readBodyParam "shares"
  publishKey bbKey public (Shares shares)

publishKey bbKey public (Shares shares) = do
  bbResult <- postpone . post bbKey . fromString $ "public key " <> show (public :: Integer)
  page "Shares" $ do
    table $ do
      entry "public key" public
      forM_ (assocs shares) $ \(i, e) ->
        entry ("private key share " <> fromString (show i)) e
    case bbResult of
      Left err -> do
        p $ "WARNING! This public key was not published to the bulletin board. Guru meditation: " <> fromString (Text.unpack err)
        form ! Attr.method "POST" ! action "republish.html" $ do
          hidden "public" public
          hidden "shares" shares
          input ! type_ "submit" ! value "try again"
      Right _ -> p "Public key successfully published."
  where
  entry  k v = tr (td k >> (td . fromString . show) v)
  hidden k v = input ! type_ "hidden" ! name k ! (value . fromString . show) v

registerForm = do
  domain <- asks server
  BB.Author name pub <- asks author
  page "Bulletin Board Registration" $ do
    p . fromString . unwords $
      [ "Ready to register with ", domain
      , "as", Text.unpack name
      , "with public key", show pub
      ]
    form ! Attr.method "POST" $
      Tag.div (input ! type_ "submit" ! value "register")

register = do
  BB.Author name pub <- asks author
  url <- asks (endpoint "register")
  postJSON_ url (name, pub)
  page "Registered" $ p "Registration complete. Check the BB's list of users to verify."

methodName method_ name action = (name, method method_ action)

question id defaultValue description = do
  input ! type_ "text" ! name id ! value defaultValue
  Tag.span ! Attr.for id $ description

page title content = render $ docTypeHtml ! lang "en" $ do
  Tag.title title
  content

errorUpdate v = doUpdate v >>= either (throwError . fromString . show) return

postpone :: MonadError e m => m a -> m (Either e a)
postpone act = catchError (Right `liftM` act) (return . Left)

data BBContactInfo = BBContactInfo
  { server  :: String
  , author  :: BB.Author
  , private :: PrivateKey
  } deriving (Eq, Read, Show)

post :: (MonadError Text m, MonadReader BBContactInfo m, MonadIO m, MonadState (AcidState HmacDRBG) m)
     => AcidState PossibleKey -> Text -> m ()
post bbKey msg = do
  -- zeroth step: ask the server about its public key, and double-check that it
  -- matches any previous public key we've seen from it
  url      <- asks (endpoint "pubkey")
  pub      <- getJSON url
  liftIO (update bbKey (KeyUnchanged pub)) >>= liftId

  -- first step: check that the server's latest hash is valid
  url      <- asks (endpoint "current-hash")
  current  <- getJSON url
  now      <- liftIO getCurrentTime
  liftBB $ BB.checkCurrentHash pub now BB.epsilon current

  -- second step: request that our message get posted
  url      <- asks (endpoint "post")
  priv     <- asks private
  author   <- asks author
  newMsg   <- doUpdate $ PrepareMessage priv (BB.Message msg) now author (forgetSignature current)
  accepted <- postJSON url newMsg
  liftBB $ BB.checkAcceptedMessage pub now BB.epsilon accepted

endpoint :: String -> BBContactInfo -> String
endpoint name info = "http://" <> server info <> "/" <> name <> ".json"

forgetSignature :: BB.CurrentHash -> BB.Hash
forgetSignature (BB.CurrentHash (BB.Signed { BB.message = (hash, _) })) = hash

getJSON :: (MonadError Text m, MonadIO m, FromJSON a) => String -> m a
getJSON url = do
  resp <- (liftIO . HTTP.simpleHTTP . HTTP.getRequest) url >>= liftHTTP url
  -- TODO: use liftEither/eitherDecode instead
  case decode . UTF8.fromString . HTTP.rspBody $ resp of
    Nothing -> throwError $  "Failed to decode from " <> fromString url <> ": "
                          <> (fromString . show . HTTP.rspBody) resp
    Just a  -> return a

postJSON :: (MonadError Text m, MonadIO m, ToJSON a, FromJSON b)
         => String -> a -> m b
postJSON url body = do
  resp <- postJSONRaw url body
  liftEither (\e -> fromString $ e <> " while parsing " <> HTTP.rspBody resp)
             (eitherDecode . UTF8.fromString . HTTP.rspBody $ resp)

postJSON_ :: (MonadError Text m, MonadIO m, ToJSON a)
          => String -> a -> m ()
postJSON_ url body = postJSONRaw url body >> return ()

postJSONRaw :: (MonadError Text m, MonadIO m, ToJSON a)
          => String -> a -> m (HTTP.Response String)
postJSONRaw url body = (liftIO . HTTP.simpleHTTP) req
                   >>= liftHTTP url
  where req = HTTP.postRequestWithBody url "application/json" . UTF8.toString . encode $ body

liftEither :: MonadError Text m => (e -> Text) -> Either e a -> m a
liftEither f = either (throwError . f) return

liftHTTP :: (MonadError Text m, Show e) => String -> Either e a -> m a
liftHTTP url = liftEither (\e -> "Error getting URL " <> fromString url <> ": " <> fromString (show e))

liftBB :: MonadError Text m => Either String a -> m a
liftBB = liftEither fromString

liftId :: MonadError Text m => Either Text a -> m a
liftId = liftEither Prelude.id

-- don't look, it's too boring {{{
-- state modifications during failing transactions are not preserved
transaction :: e ~ GenError => CRand HmacDRBG e a -> Update HmacDRBG (Either e a)
transaction m = do
  result <- gets (runCRand m)
  case result of
    Left e -> return (Left e)
    Right (a, g) -> put g >> return (Right a)

data BuildKeyPairTEG = BuildKeyPairTEG TEGParams
data BuildShares     = BuildShares     TEGParams Integer
data PrepareMessage  = PrepareMessage PrivateKey BB.Message UTCTime BB.Author BB.Hash
data BuildKeyPairECC = BuildKeyPairECC Curve

deriving instance Typeable SHA512
deriving instance Typeable BB.Message
deriving instance Typeable BB.NewMessage
deriving instance Typeable BB.Author
join <$> mapM (deriveSafeCopy 0 'base)
  [ ''HMAC.State, ''SHA512, ''GenError
  , ''BuildKeyPairTEG, ''BuildShares, ''PrepareMessage, ''BuildKeyPairECC
  , ''PossibleKey
  , ''TEGParams, ''TEGPublicKey, ''TEGPrivateKey, ''Shares
  , ''PublicKey, ''PrivateKey, ''Signature
  , ''BB.Message, ''BB.Author, ''BB.NewMessage, ''BB.Signed
  , ''Point, ''Curve, ''CurveBinary, ''CurveCommon, ''CurvePrime
  ]

instance Byteable Point where
  toBytes PointO = "\0"
  toBytes (Point a b) = "\1" <> strictPut (a, b)

instance Byteable PublicKey where
  toBytes (PublicKey curve q) = toBytes (curve, q)

instance Byteable Curve where
  toBytes (CurveF2m binary) = "\0" <> toBytes binary
  toBytes (CurveFP  prime ) = "\1" <> toBytes prime

instance Byteable CurveBinary where toBytes (CurveBinary n c) = toBytes (n, c)
instance Byteable CurvePrime  where toBytes (CurvePrime  n c) = toBytes (n, c)
instance Byteable CurveCommon where
  toBytes (CurveCommon a b g n h) = mconcat [toBytes a, toBytes b, toBytes g, toBytes n, toBytes h]

instance Byteable Integer where toBytes = strictPut

strictPut :: Binary.Binary a => a -> ByteString
strictPut = mconcat . toChunks . Binary.runPut . Binary.put

instance UpdateEvent BuildKeyPairTEG
instance Acid.Method BuildKeyPairTEG where
  type MethodResult BuildKeyPairTEG = Either GenError (TEGPublicKey, TEGPrivateKey)
  type MethodState  BuildKeyPairTEG = HmacDRBG

instance UpdateEvent BuildShares
instance Acid.Method BuildShares where
  type MethodResult BuildShares = Either GenError Shares
  type MethodState  BuildShares = HmacDRBG

instance UpdateEvent PrepareMessage
instance Acid.Method PrepareMessage where
  type MethodResult PrepareMessage = BB.NewMessage BB.Message BB.Author
  type MethodState  PrepareMessage = HmacDRBG

instance UpdateEvent BuildKeyPairECC
instance Acid.Method BuildKeyPairECC where
  type MethodResult BuildKeyPairECC = (PublicKey, PrivateKey)
  type MethodState  BuildKeyPairECC = HmacDRBG

instance (d ~ SHA512) => IsAcidic (HMAC.State d) where
  acidEvents = [ Acid.UpdateEvent $ \(BuildKeyPairTEG params       ) -> transaction (buildKeyPair params)
               , Acid.UpdateEvent $ \(BuildShares     params secret) -> transaction (buildShares params secret)
               , Acid.UpdateEvent $ \(PrepareMessage k msg tw w h  ) -> state (\g -> BB.prepareMessage g k msg tw w h)
               , Acid.UpdateEvent $ \(BuildKeyPairECC curve        ) -> state (\g -> ECC.generate g curve)
               ]

-- TODO: this is wildly unsafe: many methods are not implemented, and we ignore
-- potential reseeding requests entirely; however, in case these things go
-- wrong, the program simply stops (so this should not lead to a privacy leak)
instance CPRG HmacDRBG where
  cprgGenerate n g = either (error . show) Prelude.id (genBytes n g)
-- }}}
