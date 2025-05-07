module Triplet (tripletsWithSum) where

-- | Finds all Pythagorean triplets (a, b, c) such that a + b + c = sum.
-- A Pythagorean triplet satisfies a^2 + b^2 = c^2 and a < b < c.
tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n =
  [ (a, b, c)                         -- The triplet (a, b, c)
  | a <- [1 .. n `div` 3]             -- Iterate 'a' from 1 up to n/3.
                                      -- Since a < b < c, 3a < a + b + c = n, so a < n/3.
  , b <- [a + 1 .. (n - a) `div` 2]   -- Iterate 'b' from a+1.
                                      -- Since b < c, a + b + b < a + b + c = n, so a + 2b < n, thus b < (n - a) / 2.
  , let c = n - a - b                 -- Calculate 'c' based on the sum constraint a + b + c = n.
                                      -- The ranges for 'a' and 'b' ensure a < b and b < c.
  , a*a + b*b == c*c                  -- Check the Pythagorean condition.
  ]
