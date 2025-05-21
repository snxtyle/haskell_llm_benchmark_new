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

import Prelude -- Removed 'hiding (null)'
import Data.List (nub) -- Used for ensuring uniqueness when creating sets

-- | A custom set data type, represented internally as a list of unique elements.
-- The order of elements in the list does not matter for set equality.
data CustomSet a = CustomSet [a] deriving Show

-- | Two CustomSets are equal if they contain the same elements.
-- This is checked by verifying that each set is a subset of the other.
instance Eq a => Eq (CustomSet a) where
  setA == setB = isSubsetOf setA setB && isSubsetOf setB setA

-- | Deletes an element from the set.
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

-- | Returns the set difference (elements in setA but not in setB).
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) setB = CustomSet (filter (not . (`member` setB)) xs)

-- | Returns an empty set.
empty :: CustomSet a
empty = CustomSet []

-- | Creates a set from a list, ensuring all elements are unique.
fromList :: Eq a => [a] -> CustomSet a
fromList xs = CustomSet (nub xs)

-- | Inserts an element into the set. If the element is already present, the set remains unchanged.
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x set = fromList (x : toList set)

-- | Returns the intersection of two sets (elements common to both).
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) setB = CustomSet (filter (`member` setB) xs)

-- | Checks if two sets are disjoint (have no common elements).
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = CustomSet [] == intersection setA setB

-- | Checks if setA is a subset of setB.
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) setB = all (`member` setB) xs

-- | Checks if an element is a member of the set.
member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- | Checks if the set is empty.
null :: CustomSet a -> Bool
null (CustomSet xs) = Data.List.null xs -- Changed from Prelude.null to Data.List.null

-- | Returns the number of elements in the set.
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- | Converts the set to a list. The order of elements in the list is not guaranteed.
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

-- | Returns the union of two sets (all unique elements from both sets).
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = fromList (toList setA ++ toList setB)
