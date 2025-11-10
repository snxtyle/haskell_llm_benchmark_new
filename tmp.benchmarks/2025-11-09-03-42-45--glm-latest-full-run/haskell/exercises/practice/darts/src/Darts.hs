module Darts (score) where

-- | Calculates the score for a dart throw based on its coordinates.
-- The target consists of concentric circles with radii 1, 5, and 10.
-- - Inner circle (radius <= 1): 10 points
-- - Middle circle (1 < radius <= 5): 5 points
-- - Outer circle (5 < radius <= 10): 1 point
-- - Outside the target (radius > 10): 0 points
score :: Float -> Float -> Int
score x y
  | distSquared <= 1   = 10
  | distSquared <= 25  = 5
  | distSquared <= 100 = 1
  | otherwise          = 0
  where
    -- Calculate the squared distance from the origin (0,0) to avoid
    -- the cost of computing a square root.
    distSquared = x * x + y * y
