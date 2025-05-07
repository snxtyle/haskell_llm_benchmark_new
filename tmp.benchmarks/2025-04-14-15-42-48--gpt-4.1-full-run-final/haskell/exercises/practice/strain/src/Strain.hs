module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = go xs
  where
    go [] = []
    go (y:ys)
      | not (p y) = y : go ys
      | otherwise = go ys

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = go xs
  where
    go [] = []
    go (y:ys)
      | p y       = y : go ys
      | otherwise = go ys
