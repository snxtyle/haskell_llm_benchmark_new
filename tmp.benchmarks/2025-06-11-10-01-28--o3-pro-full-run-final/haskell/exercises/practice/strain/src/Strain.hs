module Strain (keep, discard) where

-- | Return a list containing those elements that satisfy the predicate.
keep :: (a -> Bool) -> [a] -> [a]
keep _ []     = []
keep p (x:xs)
  | p x       = x : keep p xs
  | otherwise =     keep p xs

-- | Return a list containing those elements that do NOT satisfy the predicate.
--   Implemented in terms of 'keep' to avoid duplicating recursion logic.
discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)
