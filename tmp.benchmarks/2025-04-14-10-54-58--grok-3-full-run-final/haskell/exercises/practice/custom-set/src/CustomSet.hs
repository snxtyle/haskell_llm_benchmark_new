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

import Prelude

-- Using a newtype wrapper around a list to represent our set
-- We'll maintain uniqueness by checking for duplicates during insertion
newtype CustomSet a = CustomSet [a] deriving (Show)

-- Custom Eq instance to compare sets regardless of element order
instance (Eq a) => Eq (CustomSet a) where
  (CustomSet xs) == (CustomSet ys) = 
    let sortedXs = sort xs
        sortedYs = sort ys
    in length xs == length ys && and (zipWith (==) sortedXs sortedYs)
    where
      sort [] = []
      sort (x:xs) = sort [y | y <- xs, y < x] ++ [x] ++ sort [y | y <- xs, y >= x]

-- Remove an element from the set
delete :: (Eq a) => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

-- Return elements in first set that are not in second set
difference :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) setB = CustomSet (filter (\x -> not (member x setB)) xs)

-- Create an empty set
empty :: CustomSet a
empty = CustomSet []

-- Create a set from a list, removing duplicates
fromList :: (Eq a) => [a] -> CustomSet a
fromList xs = CustomSet (removeDuplicates xs)
  where
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- Insert an element into the set if it's not already there
insert :: (Eq a) => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs) = if x `elem` xs then CustomSet xs else CustomSet (x:xs)

-- Return elements that are in both sets
intersection :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) setB = CustomSet (filter (\x -> member x setB) xs)

-- Check if two sets have no elements in common
isDisjointFrom :: (Eq a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) setB = not (any (\x -> member x setB) xs)

-- Check if first set is a subset of second set
isSubsetOf :: (Eq a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) setB = all (\x -> member x setB) xs

-- Check if an element is in the set
member :: (Eq a) => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- Check if the set is empty
null :: CustomSet a -> Bool
null (CustomSet xs) = Prelude.null xs

-- Return the number of elements in the set
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- Convert set to list
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

-- Combine two sets, removing duplicates
union :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = fromList (xs ++ ys)
