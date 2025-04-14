module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate _ []     = []               -- Base case: empty list returns empty list
accumulate f (x:xs) = (f x) : accumulate f xs  -- Recursive case: apply f to the head and recurse on the tail
