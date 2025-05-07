module Darts (score) where

-- | Calculate the score for a single dart throw given its Cartesian
-- coordinates (x, y). The rules are:
--
--   * Radius > 10               -> 0 points
--   * 5  < radius ≤ 10          -> 1 point
--   * 1  < radius ≤ 5           -> 5 points
--   * radius ≤ 1                -> 10 points
--
-- Boundaries are inclusive, meaning a dart landing exactly on a circle
-- edge counts as the higher score of the inner region.
score :: Float -> Float -> Int
score x y = case distance of
    r | r <= 1  -> 10
      | r <= 5  -> 5
      | r <= 10 -> 1
      | otherwise -> 0
  where
    distance = sqrt (x * x + y * y)
