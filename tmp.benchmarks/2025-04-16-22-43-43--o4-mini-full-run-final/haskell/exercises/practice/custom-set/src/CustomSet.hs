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

-- A set of unique elements, stored in a list.
newtype CustomSet a = CustomSet [a]

instance Eq a => Eq (CustomSet a) where
  (CustomSet xs) == (CustomSet ys) =
    all (`elem` ys) xs && all (`elem` xs) ys

instance Show a => Show (CustomSet a) where
  show (CustomSet xs) = "fromList " ++ show xs

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (L.delete x xs)

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (xs L.\\ ys)

empty :: CustomSet a
empty = CustomSet []

fromList :: Eq a => [a] -> CustomSet a
fromList xs = CustomSet (L.nub xs)

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x set@(CustomSet xs)
  | x `elem` xs = set
  | otherwise   = CustomSet (x : xs)

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) =
  CustomSet (xs `L.intersect` ys)

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom a b = null (intersection a b)

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

null :: CustomSet a -> Bool
null (CustomSet xs) = L.null xs

size :: CustomSet a -> Int
size (CustomSet xs) = length xs

toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = CustomSet (xs L.union ys)
