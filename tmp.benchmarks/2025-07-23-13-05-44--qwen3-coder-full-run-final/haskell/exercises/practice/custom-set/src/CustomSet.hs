module CustomSet
  ( CustomSet
  , delete
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
import qualified Prelude as P

data CustomSet a = CustomSet [a] deriving (Show)

instance Eq a => Eq (CustomSet a) where
  set1 == set2 = isSubsetOf set1 set2 && isSubsetOf set2 set1

-- Create an empty set
empty :: CustomSet a
empty = CustomSet []

-- Create a set from a list
fromList :: Eq a => [a] -> CustomSet a
fromList xs = CustomSet (removeDuplicates xs)
  where
    removeDuplicates [] = []
    removeDuplicates (y:ys) = y : removeDuplicates (filter (/= y) ys)

-- Insert an element into the set
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs)
  | x `elem` xs = CustomSet xs
  | otherwise = CustomSet (x:xs)

-- Check if an element is a member of the set
member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- Delete an element from the set
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

-- Get the size of the set
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- Check if the set is empty
null :: CustomSet a -> Bool
null (CustomSet xs) = P.null xs

-- Convert set to list
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

-- Check if one set is a subset of another
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

-- Check if two sets are disjoint
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = all (`notElem` ys) xs

-- Union of two sets
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = fromList (xs ++ ys)

-- Intersection of two sets
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = 
  fromList [x | x <- xs, x `elem` ys]

-- Difference of two sets
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = 
  fromList [x | x <- xs, x `notElem` ys]
