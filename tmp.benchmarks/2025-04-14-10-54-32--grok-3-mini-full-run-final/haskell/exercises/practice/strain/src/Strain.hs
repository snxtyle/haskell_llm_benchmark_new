module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []  -- Base case: empty list returns empty list
discard p (x:xs)   -- Recursive case
  | p x    = discard p xs  -- If predicate is True for x, skip it and recurse
  | otherwise = x : discard p xs  -- If predicate is False for x, include it and recurse

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []  -- Base case: empty list returns empty list
keep p (x:xs)   -- Recursive case
  | p x    = x : keep p xs  -- If predicate is True for x, include it and recurse
  | otherwise = keep p xs  -- If predicate is False for x, skip it and recurse
