module Darts (score) where

score :: Float -> Float -> Int
score x y
  | d <= 1 = 10
  | d <= 5 = 5
  | d <= 10 = 1
  | otherwise = 0
  where d = sqrt (x^2 + y^2)
