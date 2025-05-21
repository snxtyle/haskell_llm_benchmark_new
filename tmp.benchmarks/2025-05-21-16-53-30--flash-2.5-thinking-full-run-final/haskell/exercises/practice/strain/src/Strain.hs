module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p [] = []
discard p (x:xs)
    | p x       = discard p xs  -- If predicate is true, discard x
    | otherwise = x : discard p xs -- If predicate is false, keep x

keep :: (a -> Bool) -> [a] -> [a]
keep p [] = []
keep p (x:xs)
    | p x       = x : keep p xs  -- If predicate is true, keep x
    | otherwise = keep p xs      -- If predicate is false, discard x
