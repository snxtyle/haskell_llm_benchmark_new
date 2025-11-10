module Strain (keep, discard) where

-- | Returns a list of elements from the input list that satisfy the predicate.
-- This is implemented recursively without using the standard 'filter' function.
keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs)
    | p x       = x : keep p xs
    | otherwise = keep p xs

-- | Returns a list of elements from the input list that do not satisfy the predicate.
-- This is implemented by reusing the 'keep' function with a negated predicate.
discard :: (a -> Bool) -> [a] -> [a]
discard p xs = keep (not . p) xs
