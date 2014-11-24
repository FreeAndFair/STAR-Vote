{-# LANGUAGE
    GADTs,
    MultiParamTypeClasses
  #-}
module StarVote.Crypto.ThresholdElGamal where

import Crypto.Classes
import Crypto.Random
import Control.Monad.CryptoRandom

import Data.Array as A
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
  :: (CryptoRandomGen rng)
  => rng
  -> TEGParams
  -> Either GenError ((TEGPublicKey, TEGPrivateKey), rng)
buildKeyPair rng params = do
  let p = tegOrder params
      g = tegGenerator params
      lb = 1
      ub = p - 2
  (privateExponent, rng') <- crandomR (lb, ub) rng
  let publicKey  = TEGPublicKey  params (powerMod g privateExponent p)
      privateKey = TEGPrivateKey params privateExponent
  return ((publicKey, privateKey), rng')

-- ElGamal public-key encryption (Encryption).
-- [HAC 295] Algorithm 8.18.1
encryptAsym
  :: (CryptoRandomGen rng)
  => rng
  -> TEGPublicKey
  -> Integer
  -> Either GenError (TEGCipherText, rng)
encryptAsym rng (TEGPublicKey params halfSecret) msg = do
  let p = tegOrder params
      g = tegGenerator params
      lb = 1
      ub = p - 2
  (privateExponent, rng') <- crandomR (lb, ub) rng
  (privateExponent, rng'') <- crandomR (lb, ub) rng'

  let gamma = powerMod g privateExponent p
      delta = msg * (powerMod halfSecret privateExponent p)
  return (TEGCipherText gamma delta, rng'')

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
        gamma' = fromJust $ invertMod gamma p

-- Shamir's (t, n) threshold scheme (Setup)
-- [HAC 526] Mechanism 12.71.1
-- (!) Throws away rng due to use of `crandomRs`
buildShares
  :: (CryptoRandomGen rng)
  => rng        -- You will lose this!
  -> TEGParams
  -> Integer
  -> Shares
buildShares rng params secret =
  let
    p = tegOrder params
    n = tegTrustees params
    th = tegThreshold params
    lb = 0
    ub = p - 1
    coeffs = take (fromIntegral (th - 1)) $ crandomRs (lb, ub) rng
    poly = Polynomial $ listArray (0, th-1) (secret:coeffs)
  in
   Shares $ listArray (1, n) $ map (evalPolyMod p poly) [1..n]

-- Shamir's (t, n) threshold scheme (Pooling)
-- [HAC 526] Mechanism 12.71.2
recoverKeyFromShares
  :: TEGParams
  -> Shares
  -> Integer
recoverKeyFromShares params (Shares shares) = sum $ map term (assocs shares)
  where
    p = tegOrder params
    term (i, s) = (lagrangeBasisAt i) * s `mod` p
    lagrangeBasisAt i = product [ basisFactor
                                | j <- [lb..ub],
                                  i /= j,
                                  let x_j = shares ! j
                                      x_i = shares ! i
                                      basisFactor = x_j `div` (x_j - x_i)
                                ]
    (lb, ub) = bounds shares
