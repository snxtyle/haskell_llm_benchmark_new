module Darts (score) where

score :: Float -> Float -> Int
score x y
  | distance <= 1  = 10  -- Inner circle (radius 1)
  | distance <= 5  = 5   -- Middle circle (radius 5)
  | distance <= 10 = 1   -- Outer circle (radius 10)
  | otherwise      = 0   -- Outside target
  where
    distance = sqrt (x * x + y * y)
