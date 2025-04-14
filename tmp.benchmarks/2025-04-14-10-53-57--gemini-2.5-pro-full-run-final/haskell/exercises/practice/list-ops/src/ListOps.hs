module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )
import qualified Prelude -- Used for seq

-- | Given a function, a list, and an initial accumulator,
-- fold (reduce) each item into the accumulator from the left.
-- This is the strict version of foldl.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) = let z' = f z x in z' `Prelude.seq` foldl' f z' xs

-- | Given a function, a list, and an initial accumulator,
-- fold (reduce) each item into the accumulator from the right.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

-- | Given a list, return the total number of items within it.
length :: [a] -> Int
-- Implemented using foldl' for efficiency.
-- The lambda (\n _ -> n + 1) increments the accumulator 'n' for each element.
length = foldl' (\n _ -> n + 1) 0

-- | Given a list, return a list with all the original items, but in reversed order.
reverse :: [a] -> [a]
-- Implemented using foldl'.
-- The lambda (\acc x -> x : acc) conses each element 'x' onto the front of the accumulator 'acc'.
reverse = foldl' (\acc x -> x : acc) []

-- | Given a function and a list, return the list of the results of
-- applying the function to each item.
map :: (a -> b) -> [a] -> [b]
-- Implemented using foldr.
-- The lambda (\x ys -> f x : ys) applies 'f' to the current element 'x'
-- and conses the result onto the recursively processed tail 'ys'.
map f = foldr (\x ys -> f x : ys) []

-- | Given a predicate and a list, return the list of all items for which
-- the predicate evaluates to True.
filter :: (a -> Bool) -> [a] -> [a]
-- Implemented using foldr.
-- The lambda checks if the predicate 'p' holds for the current element 'x'.
-- If true, 'x' is consed onto the recursively processed tail 'ys'.
-- If false, the tail 'ys' is returned unchanged.
filter p = foldr (\x ys -> if p x then x : ys else ys) []

-- | Given two lists, add all items in the second list to the end of the first list.
(++) :: [a] -> [a] -> [a]
-- Implemented using foldr.
-- foldr prepends each element of the first list 'xs' to the second list 'ys'.
xs ++ ys = foldr (:) ys xs

-- | Given a series of lists, combine all items in all lists into one flattened list.
concat :: [[a]] -> [a]
-- Implemented using foldr and the previously defined (++) operator.
-- foldr concatenates each inner list with the result of concatenating the rest of the lists.
concat = foldr (++) []
