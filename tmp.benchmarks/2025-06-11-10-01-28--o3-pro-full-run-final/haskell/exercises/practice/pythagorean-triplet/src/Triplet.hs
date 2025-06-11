module Triplet (tripletsWithSum) where

-- | Given a target sum N, produce all Pythagorean triplets (a, b, c)
--   that satisfy:
--        a < b < c
--        a^2 + b^2 = c^2
--        a + b + c = N
--   The function returns the list of such triplets in ascending order of 'a',
--   then 'b', then 'c'.
tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n
  | n <= 0    = []
  | otherwise =
      [ (a, b, c)
      | a <- [1 .. upperA]
      , b <- [a + 1 .. upperB a]
      , let c = n - a - b
      , c > b
      , a * a + b * b == c * c
      ]
  where
    -- a must be less than a third of the sum because a < b < c
    upperA = n `div` 3
    -- For every chosen a, b must be less than half of the remaining sum
    -- (since b < c and c = n - a - b)
    upperB a = (n - a) `div` 2
