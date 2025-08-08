module Darts (score) where

score :: Float -> Float -> Int
score x y
  | distance <= 1  = 10  -- Inner circle
  | distance <= 5  = 5   -- Middle circle
  | distance <= 10 = 1   -- Outer circle
  | otherwise      = 0   -- Outside target
  where
    distance = sqrt (x * x + y * y)
