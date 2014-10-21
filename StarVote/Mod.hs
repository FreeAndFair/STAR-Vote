module Mod (Bound(..), Mod) where

import Control.Applicative
import Control.Arrow
import Data.Function (fix)

class Bound m where bound :: proxy m -> Integer
newtype Mod m = Mod Integer deriving (Eq, Ord)
instance Show (Mod m) where showsPrec n (Mod m) = showsPrec n m

instance Bound m => Read (Mod m) where
	readsPrec n s = first fromInteger <$> readsPrec n s

instance Bound m => Bounded (Mod m) where
	minBound = 0
	maxBound = withBound (subtract 1)

instance Bound m => Enum (Mod m) where
	pred (Mod m) = errorIfNotInRange "Enum.pred{Mod}: tried to take `pred' of minBound" (Mod (pred m))
	succ (Mod m) = errorIfNotInRange "Enum.succ{Mod}: tried to take `succ' of maxBound" (Mod (succ m))
	toEnum n     = errorIfNotInRange ("Enum.toEnum{Mod}: " ++ show n ++ " not in range") (Mod (fromIntegral n))
	fromEnum (Mod m) = fromEnum m
	enumFrom     m   = enumFromTo m maxBound
	enumFromThen m n | m <= n    = enumFromThenTo m n maxBound
	                 | otherwise = enumFromThenTo m n minBound
	enumFromTo     (Mod min) (Mod next)           = Mod <$> enumFromTo     min next
	enumFromThenTo (Mod min) (Mod next) (Mod max) = Mod <$> enumFromThenTo min next max

instance Bound m => Num (Mod m) where
	(+) = liftMod2 (+)
	(*) = liftMod2 (*)
	(-) = liftMod2 (-)
	negate = liftMod1 negate
	abs    = liftMod1 abs
	signum = liftMod1 signum
	fromInteger = liftMod0

inRange n = minBound <= n && n <= maxBound
errorIfNotInRange s n = if inRange n then n else error s

withBound f = fix (Mod . f . bound)
liftMod0 op                 = withBound (mod  op     )
liftMod1 op (Mod a)         = withBound (mod (op a  ))
liftMod2 op (Mod a) (Mod b) = withBound (mod (op a b))
