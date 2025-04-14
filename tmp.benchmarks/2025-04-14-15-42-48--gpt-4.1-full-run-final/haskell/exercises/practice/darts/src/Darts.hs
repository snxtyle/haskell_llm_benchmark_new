module Darts (score) where

score :: Float -> Float -> Int
score x y
  | distSq > 100 = 0
  | distSq > 25  = 1
  | distSq > 1   = 5
  | otherwise    = 10
  where
    distSq = x * x + y * y
