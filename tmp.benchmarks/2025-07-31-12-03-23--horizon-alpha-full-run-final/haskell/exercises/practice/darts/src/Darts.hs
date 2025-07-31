module Darts (score) where

score :: Float -> Float -> Int
score x y
  | d2 <= rInner2 = 10
  | d2 <= rMiddle2 = 5
  | d2 <= rOuter2 = 1
  | otherwise = 0
  where
    d2 = x * x + y * y
    rInner2 = 1 * 1
    rMiddle2 = 5 * 5
    rOuter2 = 10 * 10
