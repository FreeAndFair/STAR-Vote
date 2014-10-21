module Application.Star.Mod (Bound(..), Mod) where

import Control.Applicative
import Control.Arrow
import Data.Function (fix)
import System.Random

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
  enumFromTo     (Mod from_) (Mod to_)             = Mod <$> enumFromTo     from_ to_
  enumFromThenTo (Mod from_) (Mod then_) (Mod to_) = Mod <$> enumFromThenTo from_ then_ to_

instance Bound m => Num (Mod m) where
  (+) = liftMod2 (+)
  (*) = liftMod2 (*)
  (-) = liftMod2 (-)
  negate = liftMod1 negate
  abs    = liftMod1 abs
  signum = liftMod1 signum
  fromInteger = liftMod0

instance Bound m => Random (Mod m) where
  randomR (Mod m, Mod n) g = first Mod (randomR (m, n) g)
  random = randomR (minBound, maxBound)

inRange :: Bound m => Mod m -> Bool
inRange n = minBound <= n && n <= maxBound

errorIfNotInRange :: Bound m => String -> Mod m -> Mod m
errorIfNotInRange s n = if inRange n then n else error s

withBound :: Bound m => (Integer -> Integer) -> Mod m
withBound f = fix (Mod . f . bound)

liftMod0 :: Bound m =>                        Integer  ->                    Mod m
liftMod1 :: Bound m => (           Integer -> Integer) -> (         Mod m -> Mod m)
liftMod2 :: Bound m => (Integer -> Integer -> Integer) -> (Mod m -> Mod m -> Mod m)
liftMod0 op                 = withBound (mod  op     )
liftMod1 op (Mod a)         = withBound (mod (op a  ))
liftMod2 op (Mod a) (Mod b) = withBound (mod (op a b))
