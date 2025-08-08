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
import qualified Data.List as List

-- CustomSet implemented as a list with no duplicates
newtype CustomSet a = CustomSet [a] deriving (Show)

-- Custom Eq instance that compares sets based on elements, not order
instance Eq a => Eq (CustomSet a) where
  (CustomSet xs) == (CustomSet ys) = 
    length xs == length ys && all (`elem` ys) xs

-- Remove an element from the set
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (List.delete x xs)

-- Elements in the first set but not in the second
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet [x | x <- xs, x `notElem` ys]

-- Create an empty set
empty :: CustomSet a
empty = CustomSet []

-- Create a set from a list (removing duplicates)
fromList :: Eq a => [a] -> CustomSet a
fromList xs = CustomSet (List.nub xs)

-- Insert an element into the set
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs)
  | x `elem` xs = CustomSet xs
  | otherwise = CustomSet (x:xs)

-- Elements that are in both sets
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet [x | x <- xs, x `elem` ys]

-- Check if two sets have no elements in common
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = List.null [x | x <- xs, x `elem` ys]

-- Check if all elements of the first set are in the second set
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

-- Check if an element is in the set
member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- Check if the set is empty
null :: CustomSet a -> Bool
null (CustomSet xs) = List.null xs

-- Get the number of elements in the set
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- Convert the set to a list
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

-- Combine two sets (removing duplicates)
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = CustomSet (List.nub (xs ++ ys))
