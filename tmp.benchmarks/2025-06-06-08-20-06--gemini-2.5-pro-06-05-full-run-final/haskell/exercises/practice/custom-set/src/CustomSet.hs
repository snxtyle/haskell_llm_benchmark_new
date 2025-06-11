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
import Data.List (sort, group)

-- A CustomSet is represented as a sorted list of unique elements.
-- Using a newtype to hide the implementation details.
-- The derived Eq instance works because the internal list is always sorted.
newtype CustomSet a = CustomSet [a] deriving (Show, Eq)

-- Helper to create a sorted list with unique elements from a list.
mkSet :: Ord a => [a] -> [a]
mkSet = map head . group . sort

delete :: Ord a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (delete' x xs)
  where
    delete' :: Ord a => a -> [a] -> [a]
    delete' _ [] = []
    delete' z (y:ys) = case compare z y of
      LT -> y:ys
      EQ -> ys
      GT -> y : delete' z ys

difference :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (difference' xs ys)
  where
    difference' :: Ord a => [a] -> [a] -> [a]
    difference' xs' [] = xs'
    difference' [] _ = []
    difference' (x:xs') (y:ys') = case compare x y of
      LT -> x : difference' xs' (y:ys')
      EQ -> difference' xs' ys'
      GT -> difference' (x:xs') ys'

empty :: CustomSet a
empty = CustomSet []

fromList :: Ord a => [a] -> CustomSet a
fromList = CustomSet . mkSet

insert :: Ord a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs) = CustomSet (insert' x xs)
  where
    insert' :: Ord a => a -> [a] -> [a]
    insert' z [] = [z]
    insert' z (y:ys) = case compare z y of
      LT -> z : y : ys
      EQ -> y : ys
      GT -> y : insert' z ys

intersection :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (intersection' xs ys)
  where
    intersection' :: Ord a => [a] -> [a] -> [a]
    intersection' [] _ = []
    intersection' _ [] = []
    intersection' (x:xs') (y:ys') = case compare x y of
      LT -> intersection' xs' (y:ys')
      EQ -> x : intersection' xs' ys'
      GT -> intersection' (x:xs') ys'

isDisjointFrom :: Ord a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = disjoint' xs ys
  where
    disjoint' :: Ord a => [a] -> [a] -> Bool
    disjoint' [] _ = True
    disjoint' _ [] = True
    disjoint' (x:xs') (y:ys') = case compare x y of
      LT -> disjoint' xs' (y:ys')
      EQ -> False
      GT -> disjoint' (x:xs') ys'

isSubsetOf :: Ord a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = subset' xs ys
  where
    subset' :: Ord a => [a] -> [a] -> Bool
    subset' [] _ = True
    subset' _ [] = False
    subset' (x:xs') (y:ys') = case compare x y of
      LT -> False
      EQ -> subset' xs' ys'
      GT -> subset' (x:xs') ys'

member :: Ord a => a -> CustomSet a -> Bool
member x (CustomSet xs) = member' x xs
  where
    member' :: Ord a => a -> [a] -> Bool
    member' _ [] = False
    member' z (y:ys) = case compare z y of
      LT -> False
      EQ -> True
      GT -> member' z ys

null :: CustomSet a -> Bool
null (CustomSet xs) = xs == []

size :: CustomSet a -> Int
size (CustomSet xs) = length xs

toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = CustomSet (union' xs ys)
  where
    union' :: Ord a => [a] -> [a] -> [a]
    union' xs' [] = xs'
    union' [] ys' = ys'
    union' (x:xs') (y:ys') = case compare x y of
      LT -> x : union' xs' (y:ys')
      EQ -> x : union' xs' ys'
      GT -> y : union' (x:xs') ys'
