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
length xs = foldl' (\n _ -> n + 1) 0 xs

reverse :: [a] -> [a]
reverse xs = foldl' (flip (:)) [] xs

map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\a bs -> f a : bs) [] xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = foldr (\a bs -> if p a then a : bs else bs) [] xs

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss
