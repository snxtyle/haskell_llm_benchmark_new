module Accumulate (accumulate) where

-- | Apply a function to every element of a list, producing a new list
--   with the results. Implemented without relying on the standard
--   'map' (or similar) helpers.
accumulate :: (a -> b) -> [a] -> [b]
accumulate _ []     = []
accumulate f (x:xs) = f x : accumulate f xs
