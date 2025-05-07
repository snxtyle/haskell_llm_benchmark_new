module Strain (keep, discard) where

-- | Return all elements that satisfy the predicate.
--   Implemented without using 'filter' from the Prelude,
--   as the exercise explicitly forbids it.
keep :: (a -> Bool) -> [a] -> [a]
keep _ []     = []
keep p (x:xs)
  | p x       = x : keep p xs
  | otherwise =     keep p xs

-- | Return all elements that DO NOT satisfy the predicate.
--   Likewise implemented without relying on Prelude's 'filter'.
discard :: (a -> Bool) -> [a] -> [a]
discard _ []     = []
discard p (x:xs)
  | p x       =     discard p xs
  | otherwise = x : discard p xs
