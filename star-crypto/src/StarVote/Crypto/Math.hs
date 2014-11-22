module StarVote.Crypto.Math where

import Data.Array as A
import Data.Bits

-- Represents a polynomial in Z or Z/mZ.
-- (!) Interpretation of the specific ring is determined by the caller.
data Polynomial = Polynomial (Array Integer Integer)

-- Evaluate a polynomial, applying `f` to each evaluated term before summing
-- and to the sum itself.
evalPolyWith f (Polynomial poly) x = f $ sum [ f (coeff * x^exp)
                                             | (exp, coeff) <- assocs poly
                                             ]

-- Usual univariate polynomial evaluation.
evalPoly = evalPolyWith id

-- Evaluation of polynomial in the ring Z/mZ.
evalPolyMod m = evalPolyWith (`mod` m)
