module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sumN =
  [ (a, b, c)
  | a <- [1 .. sumN `div` 3]
  , b <- [a + 1 .. sumN `div` 2]
  , let c = sumN - a - b
  , a * a + b * b == c * c
  ]
