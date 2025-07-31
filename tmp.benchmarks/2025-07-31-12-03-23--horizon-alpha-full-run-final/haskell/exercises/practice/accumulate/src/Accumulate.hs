module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f xs = go xs
  where
    go []     = []
    go (y:ys) = f y : go ys
