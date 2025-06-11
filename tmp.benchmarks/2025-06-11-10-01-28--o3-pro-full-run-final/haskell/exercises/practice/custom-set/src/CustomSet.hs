{-# LANGUAGE DeriveFunctor #-}

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

-- The set is internally represented as a list that never contains duplicates.
-- Order does not matter, but we keep the first occurrence of every element
-- returned by `fromList` to make `show` deterministic.
newtype CustomSet a = CS { unCS :: [a] }
  deriving (Functor)

-- Equality of sets is defined by mutual subset checking (order-independent).
instance (Eq a) => Eq (CustomSet a) where
  a == b = isSubsetOf a b && isSubsetOf b a

-- A simple Show instance that resembles the constructor used by `fromList`.
instance (Show a) => Show (CustomSet a) where
  show = ("fromList " ++) . show . toList

-------------------------------------------------------------------------------
-- Internal helpers
-------------------------------------------------------------------------------
-- Remove duplicated elements while keeping the first occurrence.
normalize :: Eq a => [a] -> [a]
normalize = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------
empty :: CustomSet a
empty = CS []

fromList :: Eq a => [a] -> CustomSet a
fromList = CS . normalize

-------------------------------------------------------------------------------
-- Basic queries
-------------------------------------------------------------------------------
toList :: CustomSet a -> [a]
toList = unCS

size :: CustomSet a -> Int
size = length . unCS

null :: CustomSet a -> Bool
null = L.null . unCS

member :: Eq a => a -> CustomSet a -> Bool
member x = elem x . unCS

-------------------------------------------------------------------------------
-- Modifying operations
-------------------------------------------------------------------------------
insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x s
  | member x s = s
  | otherwise  = CS (x : unCS s)

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x = CS . filter (/= x) . unCS

-------------------------------------------------------------------------------
-- Set operations
-------------------------------------------------------------------------------
union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union a b = CS . normalize $ unCS a ++ unCS b

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection a b = CS [x | x <- unCS a, x `elem` unCS b]

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference a b = CS [x | x <- unCS a, x `notElem` unCS b]

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf a b = all (`elem` unCS b) (unCS a)

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom a b = not . any (`elem` unCS b) $ unCS a
