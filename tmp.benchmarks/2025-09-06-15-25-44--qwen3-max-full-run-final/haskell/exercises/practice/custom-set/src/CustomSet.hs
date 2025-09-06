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

import Prelude hiding ()  -- Removed 'null' from hiding list
import Data.List (nub)

data CustomSet a = CustomSet [a] deriving (Show)

instance Eq a => Eq (CustomSet a) where
  (CustomSet xs) == (CustomSet ys) = nub xs == nub ys

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet $ filter (/= x) xs

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) = CustomSet $ filter (`notElem` ys) xs

empty :: CustomSet a
empty = CustomSet []

fromList :: Ord a => [a] -> CustomSet a
fromList xs = CustomSet $ nub $ sortDedupe xs
  where sortDedupe = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs) = if x `elem` xs then CustomSet xs else CustomSet (x:xs)

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) (CustomSet ys) = CustomSet $ filter (`elem` ys) xs

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) (CustomSet ys) = null $ intersection (CustomSet xs) (CustomSet ys)

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) (CustomSet ys) = all (`elem` ys) xs

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

null :: CustomSet a -> Bool
null (CustomSet xs) = Prelude.null xs

size :: CustomSet a -> Int
size (CustomSet xs) = length $ nub xs

toList :: CustomSet a -> [a]
toList (CustomSet xs) = nub xs

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) (CustomSet ys) = CustomSet $ nub $ xs ++ ys
