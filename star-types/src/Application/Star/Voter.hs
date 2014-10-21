{-# LANGUAGE EmptyDataDecls #-}
module Application.Star.Voter where

data Voter
data VoterStatus = Voted | Hasn't deriving (Bounded, Enum, Eq, Ord, Read, Show)
