module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)
import qualified Data.List as L

-- We represent the set as a sorted list without duplicates.
newtype CustomSet a = CS [a] deriving (Eq, Show)

-- Internal helper: merge-insert preserving order and uniqueness.
ins :: Ord a => a -> [a] -> [a]
ins x [] = [x]
ins x ys@(y:ys')
  | x < y     = x : ys
  | x == y    = ys
  | otherwise = y : ins x ys'

-- Internal helper: delete one element (if present).
del :: Ord a => a -> [a] -> [a]
del _ [] = []
del x (y:ys)
  | x < y     = y:ys
  | x == y    = ys
  | otherwise = y : del x ys

-- Internal helper: membership via ordered search.
mem :: Ord a => a -> [a] -> Bool
mem _ [] = False
mem x (y:ys)
  | x < y     = False
  | x == y    = True
  | otherwise = mem x ys

-- Internal helper: union of two sorted unique lists.
uni :: Ord a => [a] -> [a] -> [a]
uni xs [] = xs
uni [] ys = ys
uni xs@(x:xt) ys@(y:yt)
  | x < y     = x : uni xt ys
  | x == y    = x : uni xt yt
  | otherwise = y : uni xs yt

-- Internal helper: intersection of two sorted unique lists.
inter :: Ord a => [a] -> [a] -> [a]
inter [] _ = []
inter _ [] = []
inter xs@(x:xt) ys@(y:yt)
  | x < y     = inter xt ys
  | x == y    = x : inter xt yt
  | otherwise = inter xs yt

-- Internal helper: difference xs \ ys
diff :: Ord a => [a] -> [a] -> [a]
diff [] _ = []
diff xs [] = xs
diff xs@(x:xt) ys@(y:yt)
  | x < y     = x : diff xt ys
  | x == y    = diff xt yt
  | otherwise = diff xs yt

-- Internal helper: subset check
subset :: Ord a => [a] -> [a] -> Bool
subset [] _ = True
subset _ [] = False
subset xs@(x:xt) ys@(y:yt)
  | x < y     = False
  | x == y    = subset xt yt
  | otherwise = subset xs yt

-- Public API

delete :: Ord a => a -> CustomSet a -> CustomSet a
delete x (CS xs) = CS (del x xs)

difference :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
difference (CS a) (CS b) = CS (diff a b)

empty :: CustomSet a
empty = CS []

fromList :: Ord a => [a] -> CustomSet a
fromList = CS . foldr ins []

insert :: Ord a => a -> CustomSet a -> CustomSet a
insert x (CS xs) = CS (ins x xs)

intersection :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CS a) (CS b) = CS (inter a b)

isDisjointFrom :: Ord a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CS a) (CS b) = null (CS (inter a b))

isSubsetOf :: Ord a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CS a) (CS b) = subset a b

member :: Ord a => a -> CustomSet a -> Bool
member x (CS xs) = mem x xs

null :: CustomSet a -> Bool
null (CS xs) = L.null xs

size :: CustomSet a -> Int
size (CS xs) = length xs

toList :: CustomSet a -> [a]
toList (CS xs) = xs

union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union (CS a) (CS b) = CS (uni a b)
