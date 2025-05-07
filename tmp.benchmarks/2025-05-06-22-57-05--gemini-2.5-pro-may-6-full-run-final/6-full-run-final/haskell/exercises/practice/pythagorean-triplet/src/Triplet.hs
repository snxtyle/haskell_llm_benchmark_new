module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n =
  [ (a, b, c)
  | a <- [1 .. (n - 3) `div` 3]
  , b <- [a + 1 .. (n - a - 1) `div` 2]
  , let c = n - a - b
  -- The condition a < b is ensured by b's starting range a + 1.
  -- The condition b < c is ensured by b's upper range (n - a - 1) `div` 2.
  -- Proof for b < c:
  -- c = n - a - b. We need to show b < n - a - b, which simplifies to 2*b < n - a.
  -- The upper bound for b is (n - a - 1) `div` 2.
  -- If n - a is even, let n - a = 2k. Then b <= (2k - 1) `div` 2 = k - 1.
  -- So, b <= (n - a)/2 - 1. This implies 2*b <= n - a - 2, so 2*b < n - a.
  -- If n - a is odd, let n - a = 2k + 1. Then b <= (2k + 1 - 1) `div` 2 = k.
  -- So, b <= (n - a - 1)/2. This implies 2*b <= n - a - 1, so 2*b < n - a.
  -- In both cases, 2*b < n - a, which means b < c.
  , a*a + b*b == c*c
  ]
