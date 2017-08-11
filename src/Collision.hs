module Collision
( Collision.all
, Collision.first
, Collision.any
, Collision.Hit(..)
) where

import qualified Data.List as List

data Hit = Albr | Arbl | Acbw | Awbc | Awbw deriving (Eq, Show)
type Segment = (Int, Int)
type CollisionCheck = Segment -> Segment -> [Hit]

-- Check whether a's right / b's left is hit
arbl :: CollisionCheck
arbl a b =
  if
    (snd b) >= (snd a) &&
    (fst b) <= (snd a) &&
    (fst b) >= (fst a) &&
    (awbw a b) == []
    then [Arbl]
    else []

-- Check whether a's left / b's right is hit
albr :: CollisionCheck
albr a b =
  if
    (fst b) <= (fst a) &&
    (snd b) >= (fst a) &&
    (snd b) <= (snd a) &&
    (awbw a b) == []
    then [Albr]
    else []

-- Check whether a's center / b's whole is hit
acbw :: CollisionCheck
acbw a b =
  if
    (fst b) >= (fst a) &&
    (snd b) <= (snd a) &&
    (awbw a b) == []
    then [Acbw]
    else []

-- Check whether a's whole / b's center is hit
awbc :: CollisionCheck
awbc a b =
  if
    (fst b) <= (fst a) &&
    (snd b) >= (snd a) &&
    (awbw a b) == []
    then [Awbc]
    else []

-- Check whether a's whole / b's whole is hit
awbw :: CollisionCheck
awbw a b =
  if
    (fst b) == (fst a) &&
    (snd b) == (snd a)
    then [Awbw]
    else []

-- a list of CollisionChecks
collisionChecks :: [CollisionCheck]
collisionChecks = [arbl, albr, acbw, awbc, awbw]

-- All potential collisions between a and b
all :: Segment -> Segment -> [Hit]
all a b = List.foldl (\acc f -> acc ++ (f a b)) [] collisionChecks

-- Finds the first collision between two segments against the checks
strgl :: [Segment] -> [CollisionCheck] -> [Hit] -> [Hit]
strgl [seg1,seg2] [] [] = []
strgl [seg1,seg2] (sln:slns) [] = strgl [seg1,seg2] slns (sln seg1 seg2)
strgl [seg1,seg2] slns acc = acc

-- Finds the first collision between two segments
first :: Segment -> Segment -> [Hit]
first a b = strgl [a, b] collisionChecks []

-- Checks whether theres any collision between two segments
any :: Segment -> Segment -> Bool
any a b = List.length (Collision.first a b) > 0
