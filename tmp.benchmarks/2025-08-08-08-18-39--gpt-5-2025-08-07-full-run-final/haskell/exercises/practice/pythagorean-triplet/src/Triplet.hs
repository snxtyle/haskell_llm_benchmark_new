module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum s
  | s <= 0 = []
  | otherwise =
      [ (a, b, c)
      | a <- [1 .. s `div` 3]
      , b <- [a + 1 .. (s - a) `div` 2]
      , let c = s - a - b
      , a * a + b * b == c * c
      ]
