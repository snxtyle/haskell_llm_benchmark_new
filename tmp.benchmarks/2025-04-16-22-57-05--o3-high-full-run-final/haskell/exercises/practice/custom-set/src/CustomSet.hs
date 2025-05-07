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
import qualified Prelude as P

-- | A very small custom set implementation backed by a list.
--   Duplicates are never stored.
newtype CustomSet a = CustomSet { getElements :: [a] }
  deriving (Show)

-- Two sets are equal when they contain exactly the same elements,
-- irrespective of order.
instance Eq a => Eq (CustomSet a) where
  a == b = isSubsetOf a b && isSubsetOf b a

-- | Construct an empty set.
empty :: CustomSet a
empty = CustomSet []

-- | Check if a set is empty.
null :: CustomSet a -> Bool
null (CustomSet xs) = P.null xs

-- | Create a set from a (potentially duplicate‑filled) list.
fromList :: Eq a => [a] -> CustomSet a
fromList = CustomSet . foldr insertUnique []
  where
    insertUnique x acc = if x `elem` acc then acc else x : acc

-- | Convert a set back to a list (order is unspecified).
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

-- | Number of elements in the set.
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- | Determine whether an element is present in the set.
member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- | Insert an element into the set.
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x s@(CustomSet xs)
  | x `elem` xs = s
  | otherwise   = CustomSet (x : xs)

-- | Remove an element from the set.
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

-- | Set union.
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) =
  CustomSet (foldr (\y acc -> if y `elem` acc then acc else y : acc) xs ys)

-- | Set intersection.
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) =
  CustomSet [x | x <- xs, x `elem` ys]

-- | Set difference (elements in the first set that are *not* in the second).
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) =
  CustomSet [x | x <- xs, x `notElem` ys]

-- | Is the first set a subset of the second?
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) setB = all (`member` setB) xs

-- | Are the two sets disjoint (no elements in common)?
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = null (intersection setA setB)
