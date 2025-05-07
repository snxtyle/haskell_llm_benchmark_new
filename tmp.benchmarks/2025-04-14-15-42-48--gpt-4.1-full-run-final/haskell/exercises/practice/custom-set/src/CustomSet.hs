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

-- | CustomSet is a wrapper around a list with unique elements.
--   The list is always kept sorted and without duplicates.
newtype CustomSet a = CustomSet [a]
  deriving (Eq, Show)

-- | Remove an element from the set.
delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (L.delete x xs)

-- | Set difference: elements in setA not in setB.
difference :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (xs L.\\ ys)

-- | The empty set.
empty :: CustomSet a
empty = CustomSet []

-- | Create a set from a list (removes duplicates, sorts).
fromList :: (Ord a) => [a] -> CustomSet a
fromList = CustomSet . L.nub . L.sort

-- | Insert an element into the set.
insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs)
  | x `elem` xs = CustomSet xs
  | otherwise   = CustomSet (L.insert x xs)

-- | Set intersection.
intersection :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (xs `L.intersect` ys)

-- | True if sets have no elements in common.
isDisjointFrom :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = L.null (xs `L.intersect` ys)

-- | True if setA is a subset of setB.
isSubsetOf :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

-- | True if element is in the set.
member :: (Ord a) => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- | True if the set is empty.
null :: CustomSet a -> Bool
null (CustomSet xs) = L.null xs

-- | Number of elements in the set.
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- | Convert set to list.
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

-- | Set union.
union :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = fromList (xs ++ ys)
