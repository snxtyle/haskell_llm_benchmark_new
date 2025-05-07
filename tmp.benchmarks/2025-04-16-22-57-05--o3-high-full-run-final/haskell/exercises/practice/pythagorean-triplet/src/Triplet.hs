module Triplet (tripletsWithSum) where

-- | Produce all Pythagorean triplets (a, b, c) that satisfy
--
--     a < b < c
--     a² + b² = c²
--     a + b + c = given sum
--
-- The resulting list is ordered by increasing a, then b.
tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum s
  | s <= 0    = []
  | otherwise =
      [ (a, b, c)
      | a <- [1 .. s `div` 3]               -- a is the smallest side
      , b <- [a + 1 .. (s - a) `div` 2]     -- b is the middle side
      , let c = s - a - b                   -- c is determined by the sum
      , b < c                               -- enforce strict ordering
      , a * a + b * b == c * c              -- Pythagorean condition
      ]
