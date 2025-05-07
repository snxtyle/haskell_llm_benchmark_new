module Darts (score) where

score :: Float -> Float -> Int
score x y
  | distSq > 100 = 0 -- Outside the target (radius 10)
  | distSq > 25  = 1 -- In the outer circle (radius 5 to 10)
  | distSq > 1   = 5 -- In the middle circle (radius 1 to 5)
  | otherwise    = 10 -- In the inner circle (radius 0 to 1)
  where
    distSq = x*x + y*y -- Calculate the square of the distance from the center (0,0)
