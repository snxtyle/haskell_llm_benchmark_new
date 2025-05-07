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
  , CustomSet -- Export the type constructor as well
  ) where

import Prelude -- Remove hiding (null)
import Data.List (nub, sort) -- Import nub for uniqueness and sort for Eq instance

-- Define CustomSet as a newtype wrapper around a list of unique elements.
-- We store the list sorted to make equality comparison easier and potentially
-- improve performance of some operations (though not implemented here).
-- The Eq a constraint is needed for nub and elem.
newtype CustomSet a = CustomSet { unSet :: [a] } deriving (Show)

-- Two sets are equal if they contain the same elements.
-- We can check this efficiently if both internal lists are sorted and unique.
instance (Eq a, Ord a) => Eq (CustomSet a) where
  (CustomSet xs) == (CustomSet ys) = xs == ys -- Relies on fromList producing sorted, unique lists

-- Helper function to ensure the internal list is sorted and unique
-- Requires Ord a for sorting.
normalize :: (Eq a, Ord a) => [a] -> [a]
normalize = nub . sort

delete :: (Eq a, Ord a) => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs) -- filter preserves order

difference :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (filter (`notElem` ys) xs)

empty :: CustomSet a
empty = CustomSet []

fromList :: (Eq a, Ord a) => [a] -> CustomSet a
fromList xs = CustomSet (normalize xs)

insert :: (Eq a, Ord a) => a -> CustomSet a -> CustomSet a
insert x set@(CustomSet xs)
  | member x set = set -- If already present, return the original set
  | otherwise    = CustomSet (normalize (x:xs)) -- Otherwise, add and re-normalize

intersection :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (filter (`elem` ys) xs)

isDisjointFrom :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = CustomSet.null (intersection setA setB) -- Use qualified name to avoid ambiguity
-- Alternative implementation: check if any element of setA is in setB
-- isDisjointFrom (CustomSet xs) setB = not (any (`member` setB) xs)

isSubsetOf :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) setB = all (`member` setB) xs

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs -- Uses the list's elem

-- Define our own null function, which now clashes with Prelude.null
-- We need to use Prelude.null explicitly where needed.
null :: CustomSet a -> Bool
null (CustomSet xs) = Prelude.null xs

size :: CustomSet a -> Int
size (CustomSet xs) = length xs

toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

union :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = CustomSet (normalize (xs ++ ys))
-- A potentially more efficient union for sorted lists could be implemented,
-- but normalize handles uniqueness and sorting simply.

