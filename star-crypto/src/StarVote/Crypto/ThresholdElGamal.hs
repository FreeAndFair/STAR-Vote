{-# LANGUAGE
    GADTs,
    MultiParamTypeClasses,
    PackageImports
  #-}
module StarVote.Crypto.ThresholdElGamal where

import Crypto.Classes
import "crypto-random" Crypto.Random
import Control.Monad
import Control.Monad.CryptoRandom

import Data.Array (listArray)
import Data.Map as M hiding (map)
import Data.Maybe (fromJust)
import qualified Data.ByteString as B

import Math.NumberTheory.Moduli

import StarVote.Crypto.Math
import StarVote.Crypto.Types


-- Reference:
--   [HAC] Menezes, van Oorschot, Vanstone.
--         "Handbook of Applied Cryptography".
--          CRC Press, 1996
--          www.cacr.math.uwaterloo.ca/hac


-- Key generation for ElGamal public-key encryption.
-- [HAC 294] Algorithm 8.17
buildKeyPair
  :: MonadCRandomR e m
  => TEGParams
  -> m (TEGPublicKey, TEGPrivateKey)
buildKeyPair params = do
  let p = tegOrder params
      g = tegGenerator params
      lb = 1
      ub = p - 1
  privateExponent <- getCRandomR (lb, ub)
  let publicKey  = TEGPublicKey  params (powerMod g privateExponent p)
      privateKey = TEGPrivateKey params privateExponent
  return (publicKey, privateKey)

-- ElGamal public-key encryption (Encryption).
-- [HAC 295] Algorithm 8.18.1
encryptAsym
  :: MonadCRandomR e m
  => TEGPublicKey
  -> Integer
  -> m TEGCipherText
encryptAsym (TEGPublicKey params halfSecret) msg = do
  let p = tegOrder params
      g = tegGenerator params
      lb = 1
      ub = p - 1
  privateExponent <- getCRandomR (lb, ub)
  let gamma = powerMod g privateExponent p
      delta = msg * (powerMod halfSecret privateExponent p)
  return (TEGCipherText gamma delta)

-- ElGamal public-key encryption (Decryption).
-- [HAC 295] Algorithm 8.18.2
decryptAsym
  :: TEGPrivateKey
  -> TEGCipherText
  -> Integer
decryptAsym pk c = mod (gamma' * delta) p
  where (TEGPrivateKey params privateExponent) = pk
        (TEGCipherText gamma delta) = c
        p = tegOrder params

        g = tegGenerator params
        gamma' = fromJust $ invertMod (powerMod gamma privateExponent p) p

-- Shamir's (t, n) threshold scheme (Setup)
-- [HAC 526] Mechanism 12.71.1
buildShares
  :: MonadCRandomR e m
  => TEGParams
  -> Integer
  -> m Shares
buildShares params secret = do
  let
    p = tegOrder params
    n = tegTrustees params
    th = tegThreshold params
    lb = 0
    ub = p - 1
  coeffs <- replicateM (fromIntegral (th - 1)) $ getCRandomR (lb, ub)
  let poly = Polynomial $ listArray (0, th-1) (secret:coeffs)
  return . Shares $ fromAscList [(i, evalPolyMod p poly i) | i <- [1..n]]

-- Shamir's (t, n) threshold scheme (Pooling)
-- [HAC 526] Mechanism 12.71.2
recoverKeyFromShares
  :: TEGParams
  -> Shares
  -> Integer
recoverKeyFromShares params (Shares shares) = (sum . map term . assocs) shares `mod` p
  where
    p = tegOrder params
    a `div` b = case invertMod b p of
      Just i -> a * i
      _ -> error "attempting to recover key in non-prime order"
    term (i, s) = (lagrangeBasisAt i) * s `mod` p
                                -- NOTA BENE: `div` here is in the finite field Z/p
    lagrangeBasisAt i = product [ j `div` (j - i)
                                | j <- keys shares
                                , i /= j
                                ]
