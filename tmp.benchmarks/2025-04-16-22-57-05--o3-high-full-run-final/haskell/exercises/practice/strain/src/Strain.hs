module Strain (keep, discard) where

-- | Return a list containing the elements that satisfy the predicate.
-- Implemented without using the standard library's 'filter' function.
keep :: (a -> Bool) -> [a] -> [a]
keep p = go
  where
    go [] = []
    go (x:xs)
      | p x       = x : go xs
      | otherwise =     go xs

-- | Return a list containing the elements that do NOT satisfy the predicate.
-- Also implemented without relying on 'filter' or similar helpers.
discard :: (a -> Bool) -> [a] -> [a]
discard p = go
  where
    go [] = []
    go (x:xs)
      | p x       =     go xs
      | otherwise = x : go xs
