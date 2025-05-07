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
import qualified Data.List as DL -- Used for nub and null.

-- Define CustomSet as a newtype wrapper around a list of elements 'a'.
-- The internal list [a] is used to store the unique elements of the set.
--
-- The 'Show' instance is derived. This will make CustomSet printable in a default
-- format (e.g., CS [1,2,3]) provided 'a' is also Show.
--
-- The 'Eq' instance is provided manually to ensure correct set equality semantics
-- (i.e., two sets are equal if they contain the same elements, regardless of order).
newtype CustomSet a = CS [a] deriving (Show)

-- Eq instance for CustomSet a.
-- For two sets to be equal, they must:
-- 1. Have the same number of elements (size).
-- 2. All elements of the first set must be present in the second set.
-- This requires that the element type 'a' supports equality testing (Eq a).
instance Eq a => Eq (CustomSet a) where
  setA == setB = size setA == size setB && isSubsetOf setA setB

-- Deletes an element from the set.
-- If the element is not in the set, the set remains unchanged.
-- Requires Eq a for element comparison.
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CS xs) = CS (filter (/= x) xs)

-- Returns the difference of two sets.
-- The resulting set contains all elements that are in the first set
-- but not in the second set.
-- Requires Eq a for element comparison (via 'member').
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CS xs1) setB = CS (filter (not . member setB) xs1)

-- Returns an empty set.
empty :: CustomSet a
empty = CS []

-- Creates a set from a list of elements.
-- Duplicate elements in the input list are ignored; only unique elements
-- are included in the resulting set.
-- Requires Eq a for uniqueness checking (via 'DL.nub').
fromList :: Eq a => [a] -> CustomSet a
fromList = CS . DL.nub -- nub from Data.List removes duplicate elements.

-- Inserts an element into the set.
-- If the element is already present, the set remains unchanged.
-- Requires Eq a for element comparison (via 'member').
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x set@(CS xs)
  | member x set = set      -- Element already in set, return unchanged.
  | otherwise    = CS (x : xs) -- Prepend new element. Order doesn't matter for the set.

-- Returns the intersection of two sets.
-- The resulting set contains all elements that are common to both sets.
-- Requires Eq a for element comparison (via 'member').
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CS xs1) setB = CS (filter (`member` setB) xs1)

-- Checks if two sets are disjoint (i.e., have no elements in common).
-- Returns True if the intersection of the sets is empty, False otherwise.
-- Requires Eq a for element comparison (via 'member').
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CS xs1) setB = all (not . member setB) xs1
-- An alternative implementation: isDisjointFrom setA setB = null (intersection setA setB)

-- Checks if the first set is a subset of the second set.
-- Returns True if all elements of the first set are also present in the second set.
-- Requires Eq a for element comparison (via 'member').
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CS xs1) setB = all (`member` setB) xs1

-- Checks if an element is a member of the set.
-- Returns True if the element is in the set, False otherwise.
-- Requires Eq a for element comparison.
member :: Eq a => a -> CustomSet a -> Bool
member x (CS xs) = x `elem` xs -- 'elem' is the list membership test from Prelude.

-- Checks if the set is empty.
-- Returns True if the set contains no elements, False otherwise.
null :: CustomSet a -> Bool
null (CS xs) = DL.null xs -- 'DL.null' (Data.List.null) checks if a list is empty.

-- Returns the number of elements in the set.
-- This assumes the internal list representation (xs) maintains uniqueness,
-- which is ensured by 'fromList' and 'insert'.
size :: CustomSet a -> Int
size (CS xs) = length xs

-- Converts the set to a list of its elements.
-- The order of elements in the returned list is not specified by set semantics
-- and may depend on the history of insertions and deletions.
toList :: CustomSet a -> [a]
toList (CS xs) = xs

-- Returns the union of two sets.
-- The resulting set contains all elements that are present in either set (or both),
-- with duplicates removed.
-- Requires Eq a for element comparison (via 'insert').
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
-- A common way to implement union is to fold 'insert' over the elements of one set into the other.
-- This ensures that elements from the second set are added (uniquely) to the first.
union setA (CS xsB) = foldr insert setA xsB
-- Alternative using fromList, potentially less efficient for large sets:
-- union (CS xsA) (CS xsB) = fromList (xsA ++ xsB)
