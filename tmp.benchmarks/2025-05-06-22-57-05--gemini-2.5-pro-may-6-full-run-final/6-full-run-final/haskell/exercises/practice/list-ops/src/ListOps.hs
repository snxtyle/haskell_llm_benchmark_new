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

-- | Strict left fold.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) =
  let acc' = f acc x
  in acc' `seq` foldl' f acc' xs

-- | Right fold.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

-- | Given a list, return the total number of items within it.
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

-- | Given a list, return a list with all the original items, but in reversed order.
reverse :: [a] -> [a]
reverse = go []
  where
    go acc []     = acc
    go acc (x:xs) = go (x:acc) xs

-- | Given a function and a list, return the list of the results of applying function(item) on all items.
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- | Given a predicate and a list, return the list of all items for which predicate(item) is True.
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- | Given two lists, add all items in the second list to the end of the first list.
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- | Given a series of lists, combine all items in all lists into one flattened list.
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss
-- Alternatively, using foldr:
-- concat = foldr (++) []
