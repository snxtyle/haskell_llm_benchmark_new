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
  ( length
  , reverse
  , map
  , filter
  , foldr
  , (++)
  , concat
  )

-- | A strict left fold.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = go z xs
  where
    go acc []     = acc
    go acc (y:ys) =
      let acc' = f acc y
      in acc' `seq` go acc' ys

-- | A right fold.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

-- | Compute the length of a list.
length :: [a] -> Int
length = foldl' (\n _ -> n + 1) 0

-- | Reverse a list.
reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

-- | Map a function over a list.
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- | Filter a list using a predicate.
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- | Append two lists.
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- | Concatenate a list of lists.
concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss
