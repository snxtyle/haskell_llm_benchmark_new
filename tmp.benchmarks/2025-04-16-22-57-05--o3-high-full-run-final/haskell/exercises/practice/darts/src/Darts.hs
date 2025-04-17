module Darts (score) where

-- | Calculate the score of a single dart throw given its (x, y) coordinates.
--   The scoring rules are:
--     * 10 points for a hit inside or on the inner circle     (radius 1)
--     *  5 points for a hit inside or on the middle circle    (radius 5)
--     *  1 point  for a hit inside or on the outer circle     (radius 10)
--     *  0 points for anything outside the target
score :: Float -> Float -> Int
score x y
  | rSquared <= 1  ^ (2 :: Int) = 10  -- r ≤ 1
  | rSquared <= 5  ^ (2 :: Int) = 5   -- 1 < r ≤ 5
  | rSquared <= 10 ^ (2 :: Int) = 1   -- 5 < r ≤ 10
  | otherwise                    = 0
  where
    -- Avoid an extra sqrt by comparing squared radii
    rSquared :: Float
    rSquared = x * x + y * y
