module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)

keep :: (a -> Bool) -> [a] -> [a]
keep p = go
  where
    go [] = []
    go (x:xs)
      | p x       = x : go xs
      | otherwise = go xs
