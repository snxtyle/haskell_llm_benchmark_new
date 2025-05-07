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

-- | Strict left fold (like Data.List.foldl')
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f z (x:xs) =
  let z' = f z x
  in  z' `seq` foldl' f z' xs

-- | Right fold
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

-- | Length of a list
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

-- | Reverse a list (tail‑recursive, O(n))
reverse :: [a] -> [a]
reverse xs = go xs []
  where
    go []     acc = acc
    go (y:ys) acc = go ys (y : acc)

-- | Map over a list
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- | Filter a list with a predicate
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise =     filter p xs

-- | Append two lists
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- | Concatenate a list of lists
concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss
