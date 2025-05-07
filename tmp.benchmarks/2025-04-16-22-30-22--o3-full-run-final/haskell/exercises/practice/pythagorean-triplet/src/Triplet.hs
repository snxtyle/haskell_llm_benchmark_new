module Triplet (tripletsWithSum) where

-- | Find all Pythagorean triplets (a, b, c) such that
--   a < b < c
--   a² + b² = c²
--   a + b + c = given sum
--
--   We search by enumerating possible a and b, then
--   derive c = sum - a - b and test the Pythagorean
--   condition.  Bounds:
--     • a < b < c  ⇒  a < sum / 3, b < (sum - a) / 2
--
--   The result is returned in ascending order of a, then b.
tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum s =
  [ (a, b, c)
  | a <- [1 .. s `div` 3]
  , b <- [a + 1 .. (s - a) `div` 2]
  , let c = s - a - b
  , a * a + b * b == c * c
  ]
