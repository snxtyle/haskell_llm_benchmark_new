module Accumulate (accumulate) where

-- | Apply a function to every element of a list, returning a new list of the
-- results.
--
-- This is deliberately implemented without using Prelude's 'map' (or any
-- other higher-order helper such as 'fmap') to satisfy the exercise
-- restrictions.
accumulate :: (a -> b) -> [a] -> [b]
accumulate f = go
  where
    go []     = []
    go (x:xs) = f x : go xs
