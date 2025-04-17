module Accumulate (accumulate) where

-- | Apply a function to every element in a list, producing a new list
--   of the results.  Implemented without using Prelude's 'map', 'fmap',
--   or other higher‑order mapping helpers.
accumulate :: (a -> b) -> [a] -> [b]
accumulate _ []     = []
accumulate f (x:xs) = f x : accumulate f xs
