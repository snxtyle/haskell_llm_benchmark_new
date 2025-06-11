module Darts (score) where

-- | Calculate the dart score given its landing coordinates.
--   The scoring is based on the distance from the origin (0,0):
--     * 10 points  -> distance ≤ 1
--     * 5  points  -> distance ≤ 5
--     * 1  point   -> distance ≤ 10
--     * 0  points  -> distance  > 10
--
--   The function treats the circle boundaries as inclusive, meaning that
--   landing exactly on a boundary counts for the higher-value circle.
score :: Float -> Float -> Int
score x y
  | distanceSquared <= innerR2  = 10
  | distanceSquared <= middleR2 = 5
  | distanceSquared <= outerR2  = 1
  | otherwise                   = 0
  where
    distanceSquared = x * x + y * y  -- avoid an expensive sqrt
    innerR2  = 1  * 1   -- radius^2 of inner circle
    middleR2 = 5  * 5   -- radius^2 of middle circle
    outerR2  = 10 * 10  -- radius^2 of outer circle
