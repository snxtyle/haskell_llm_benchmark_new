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
foldl' _ z [] = z
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

length :: [a] -> Int
length xs = foldl' (\acc _ -> acc + 1) 0 xs

reverse :: [a] -> [a]
reverse xs = foldl' (flip (:)) [] xs

map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\x acc -> f x : acc) [] xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss
