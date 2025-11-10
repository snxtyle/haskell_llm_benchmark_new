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
import Data.List (nub, sort)

-- A CustomSet is a newtype wrapper around a list to ensure uniqueness of elements.
newtype CustomSet a = CustomSet [a] deriving (Show)

-- Two sets are equal if they contain the same elements, regardless of order.
instance Eq a => Eq (CustomSet a) where
  (CustomSet setA) == (CustomSet setB) = sort setA == sort setB

-- Deletes an element from the set, if present.
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

-- Returns a set of all elements in the first set that are not in the second set.
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = fromList (filter (\x -> not (member x setB)) (toList setA))

-- Creates an empty set.
empty :: CustomSet a
empty = CustomSet []

-- Creates a set from a list, removing duplicate elements.
fromList :: Eq a => [a] -> CustomSet a
fromList xs = CustomSet (nub xs)

-- Inserts an element into the set.
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x set = fromList (x : toList set)

-- Returns a set of elements common to both sets.
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = fromList (filter (`member` setB) (toList setA))

-- Checks if two sets have no elements in common.
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = all (\x -> not (member x setB)) (toList setA)

-- Checks if the first set is a subset of the second set.
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = all (`member` setB) (toList setA)

-- Checks if an element is a member of the set.
member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- Checks if the set is empty.
null :: CustomSet a -> Bool
null (CustomSet xs) = xs == []

-- Returns the number of elements in the set.
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- Converts the set to a list.
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

-- Returns a set containing all elements from both sets.
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = fromList (toList setA ++ toList setB)
