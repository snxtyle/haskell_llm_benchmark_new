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

newtype CustomSet a = CustomSet [a] deriving (Eq, Show)

-- Helper: ensure uniqueness in list
unique :: Eq a => [a] -> [a]
unique = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet (filter (`notElem` ys) xs)

empty :: CustomSet a
empty = CustomSet []

fromList :: Eq a => [a] -> CustomSet a
fromList xs = CustomSet (unique xs)

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs)
  | x `elem` xs = CustomSet xs
  | otherwise = CustomSet (x : xs)

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet (filter (`elem` ys) xs)

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = null (intersection (CustomSet xs) (CustomSet ys))

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

null :: CustomSet a -> Bool
null (CustomSet xs) = List.null xs

size :: CustomSet a -> Int
size (CustomSet xs) = length xs

toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = CustomSet (unique (xs ++ ys))
