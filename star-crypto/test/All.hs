module Main where

import Test.QuickCheck
import StarVote.Crypto.Math

expModCorrect m b e = expMod m b e == 1 + b^e `mod` m

main :: IO ()
main = quickCheck expModCorrect
