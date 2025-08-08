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

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = go z xs
  where
    go acc []     = acc
    go acc (y:ys) =
      let acc' = f acc y
       in acc' `seq` go acc' ys

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

length :: [a] -> Int
length xs = go 0 xs
  where
    go n []     = n
    go n (_:ys) =
      let n' = n + 1
       in n' `seq` go n' ys

reverse :: [a] -> [a]
reverse xs = go [] xs
  where
    go acc []     = acc
    go acc (y:ys) = go (y : acc) ys

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss
