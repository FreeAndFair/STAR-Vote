{-# LANGUAGE AutoDeriveTypeable, OverloadedStrings, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}
module Main where

import Application.Star.CommonImports
import Application.Star.Util hiding (method)
import Control.Monad.CryptoRandom
import Crypto.Random.DRBG
import Crypto.Hash.CryptoAPI
import Data.Acid
import Data.SafeCopy
import Data.String
import Data.Text
import Data.Typeable
import Paths_star_crypto
import StarVote.Crypto.Groups
import StarVote.Crypto.ThresholdElGamal
import StarVote.Crypto.Types
import Text.Blaze.Html4.Strict
import Text.Blaze.Html4.Strict.Attributes hiding (method)

import qualified Data.Acid.Advanced as Acid
import qualified Text.Blaze.Html4.Strict as Tag
import qualified Text.Blaze.Html4.Strict.Attributes as Attr
import qualified Crypto.Random.DRBG.HMAC as HMAC

main :: IO ()
main = do
  -- TODO: It would be nice if there were a way to only generate a seed when
  -- loading from the acid-state database failed, since this probably uses up
  -- real entropy.
  seed <- newGenIO :: IO HmacDRBG
  stateFile <- getDataFileName "key-generation"
  state <- openLocalStateFrom stateFile seed
  flip statefulErrorServe state . route $
    [ methodName GET  "initialize" quorumConfiguration
    , methodName POST "initialize" generateShares
    ]

quorumConfiguration = page "Quorum Configuration" $ do
  form ! Attr.method "POST" $ do
    question "trustee_count" "1" "How many trustees are there?"
    question "threshold"     "1" "How many trustees should be required when finalizing the election?"
    Tag.div (input ! type_ "submit" ! value "generate key shares")

generateShares = do
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
  (TEGPublicKey _ public, TEGPrivateKey _ private) <- errorUpdate (BuildKeyPair params)
  shares <- errorUpdate (BuildShares params private)
  page "Shares" (fromString . show $ (public, shares))

errorUpdate v = doUpdate v >>= either (throwError . pack . show) return

methodName method_ name action = (name, method method_ action)

question id defaultValue description = do
  input ! type_ "text" ! name id ! value defaultValue
  Tag.span ! Attr.for id $ description

page title content = render $ docTypeHtml ! lang "en" $ do
  Tag.title title
  content

-- state modifications during failing transactions are not preserved
transaction :: e ~ GenError => CRand HmacDRBG e a -> Update HmacDRBG (Either e a)
transaction m = do
  result <- gets (runCRand m)
  case result of
    Left e -> return (Left e)
    Right (a, g) -> put g >> return (Right a)

data BuildKeyPair = BuildKeyPair TEGParams
data BuildShares  = BuildShares  TEGParams Integer

deriving instance Typeable SHA512
deriveSafeCopy 0 'base ''SHA512
deriveSafeCopy 0 'base ''HMAC.State
deriveSafeCopy 0 'base ''GenError
deriveSafeCopy 0 'base ''BuildKeyPair
deriveSafeCopy 0 'base ''BuildShares
deriveSafeCopy 0 'base ''Shares
deriveSafeCopy 0 'base ''TEGParams
deriveSafeCopy 0 'base ''TEGPublicKey
deriveSafeCopy 0 'base ''TEGPrivateKey

instance UpdateEvent BuildKeyPair
instance Acid.Method BuildKeyPair where
  type MethodResult BuildKeyPair = Either GenError (TEGPublicKey, TEGPrivateKey)
  type MethodState  BuildKeyPair = HmacDRBG

instance UpdateEvent BuildShares
instance Acid.Method BuildShares where
  type MethodResult BuildShares = Either GenError Shares
  type MethodState  BuildShares = HmacDRBG

instance (d ~ SHA512) => IsAcidic (HMAC.State d) where
  acidEvents = [ Acid.UpdateEvent $ \(BuildKeyPair params) -> transaction (buildKeyPair params)
               , Acid.UpdateEvent $ \(BuildShares  params secret) -> transaction (buildShares params secret)
               ]
