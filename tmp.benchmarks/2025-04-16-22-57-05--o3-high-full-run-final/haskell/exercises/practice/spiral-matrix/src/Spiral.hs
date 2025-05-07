module Spiral (spiral) where

import Data.Array (Array, array, (!))

-- | Generate a square matrix of the given size filled with the numbers
--   1 .. n² in a clockwise spiral order.
--
--   For example, `spiral 3` yields
--
--   1 2 3
--   8 9 4
--   7 6 5
--
--   For non‑positive sizes an empty matrix is returned.
spiral :: Int -> [[Int]]
spiral n
  | n <= 0    = []
  | otherwise = [[arr ! (r, c) | c <- [0 .. n - 1]] | r <- [0 .. n - 1]]
  where
    arr :: Array (Int, Int) Int
    arr = array ((0, 0), (n - 1, n - 1)) (zip (spiralCoords n) [1 .. n * n])

-- Produce the list of matrix coordinates in the desired spiral order.
-- The coordinates are zero‑based, starting at (0,0) (top‑left).
spiralCoords :: Int -> [(Int, Int)]
spiralCoords n = go True stepSizes directions (0, 0)
  where
    -- First segment has length n, then each smaller length (n‑1, n‑2, …, 1)
    -- appears twice.
    stepSizes :: [Int]
    stepSizes = n : concatMap (replicate 2) [n - 1, n - 2 .. 1]

    -- Directions for Right, Down, Left, Up cycling forever.
    directions :: [(Int, Int)]
    directions = cycle [(0, 1), (1, 0), (0, -1), (-1, 0)]

    -- Recursive helper that walks the spiral, producing all coordinates.
    -- The Bool flag indicates whether we're processing the very first segment,
    -- whose starting cell must be included.
    go :: Bool -> [Int] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    go _      []           _            _        = []
    go first (len:lens) (dir:dirs) (r, c) =
      segment ++ go False lens dirs (last segment)
      where
        (dr, dc) = dir
        -- For the first segment include the current cell (step 0),
        -- for subsequent ones start moving immediately (step 1).
        steps     = if first then [0 .. len - 1] else [1 .. len]
        segment   = [(r + k * dr, c + k * dc) | k <- steps]
    go _ _ _ _ = []  -- Should never happen, but keeps pattern‑matching total.
