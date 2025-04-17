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
import qualified Prelude (null)
import Data.List (nub)

-- | A very small, purely functional set implementation based on a list that
--   never contains duplicates.
newtype CustomSet a = CustomSet [a]

-- Equality is defined in terms of set semantics:
-- two sets are equal when each is a subset of the other.
instance Eq a => Eq (CustomSet a) where
  CustomSet xs == CustomSet ys = isSub xs ys && isSub ys xs
    where
      isSub as bs = all (`elem` bs) as

-- Show instance mimics the helper constructor used in tests: `fromList`.
instance Show a => Show (CustomSet a) where
  show (CustomSet xs) = "fromList " ++ show xs

-- | Create an empty set.
empty :: CustomSet a
empty = CustomSet []

-- | Build a set from a list, discarding any duplicates.
fromList :: Eq a => [a] -> CustomSet a
fromList = CustomSet . nub

-- | Convert a set back to a list.
toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

-- | Number of elements in the set.
size :: CustomSet a -> Int
size (CustomSet xs) = length xs

-- | Is the set empty?
null :: CustomSet a -> Bool
null (CustomSet xs) = Prelude.null xs

-- | Determine whether an element is a member of the set.
member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

-- | Insert an element into the set.
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs)
  | x `elem` xs = CustomSet xs
  | otherwise   = CustomSet (x : xs)

-- | Delete an element from the set.
delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

-- | Set union.
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = CustomSet (nub (xs ++ ys))

-- | Set intersection.
intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet [x | x <- xs, x `elem` ys]

-- | Set difference (elements in the first set that are not in the second).
difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet [x | x <- xs, x `notElem` ys]

-- | Is the first set a subset of the second?
isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

-- | Are the two sets disjoint?
isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = all (`notElem` ys) xs
