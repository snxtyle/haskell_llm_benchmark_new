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
foldr f z (y:ys) = f y (foldr f z ys)

length :: [a] -> Int
length xs = go 0 xs
  where
    go n []     = n
    go n (_:ys) = go (n + 1) ys

reverse :: [a] -> [a]
reverse xs = go [] xs
  where
    go acc []     = acc
    go acc (y:ys) = go (y : acc) ys

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (y:ys) = f y : map f ys

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (y:ys)
  | p y       = y : filter p ys
  | otherwise =     filter p ys

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(z:zs) ++ ys = z : (zs ++ ys)

concat :: [[a]] -> [a]
concat []       = []
concat (ys:yss) = ys ++ concat yss
