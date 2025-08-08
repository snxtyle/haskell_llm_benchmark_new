module Darts (score) where

score :: Float -> Float -> Int
score x y
  | r2 <= 1    = 10
  | r2 <= 25   = 5
  | r2 <= 100  = 1
  | otherwise  = 0
  where
    r2 = x * x + y * y
