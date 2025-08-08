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

newtype CustomSet a = CustomSet [a]

-- Two sets are equal iff they are mutual subsets.
instance Eq a => Eq (CustomSet a) where
  (==) s1 s2 = isSubsetOf s1 s2 && isSubsetOf s2 s1

-- Display as "fromList [..]" using the list representation.
instance Show a => Show (CustomSet a) where
  show s = "fromList " ++ show (toList s)

empty :: CustomSet a
empty = CustomSet []

null :: CustomSet a -> Bool
null (CustomSet xs) = L.null xs

size :: CustomSet a -> Int
size (CustomSet xs) = length xs

toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

fromList :: Eq a => [a] -> CustomSet a
fromList = foldl (flip insert) empty

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = x `elem` xs

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x (CustomSet xs)
  | x `elem` xs = CustomSet xs
  | otherwise   = CustomSet (xs ++ [x])

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet (filter (/= x) xs)

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) setB = all (`member` setB) xs

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom (CustomSet xs) setB = all (not . (`member` setB)) xs

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) setB = foldl (flip insert) setB xs

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) setB = CustomSet [x | x <- xs, member x setB]

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) setB = CustomSet [x | x <- xs, not (member x setB)]
