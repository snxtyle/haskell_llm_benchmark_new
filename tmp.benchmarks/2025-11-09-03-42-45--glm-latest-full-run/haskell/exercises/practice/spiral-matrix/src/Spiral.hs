module Spiral (spiral) where

-- | Generates a square matrix of a given size, filled with natural numbers
-- in a clockwise spiral order.
spiral :: Int -> [[Int]]
spiral size
  | size <= 0 = []
  | otherwise = buildMatrix size (allCoords size)
  where
    -- Generates the list of coordinates for the entire spiral
    allCoords :: Int -> [(Int, Int)]
    allCoords n = concatMap (getLayerCoords n) [0..n `div` 2 - 1] ++ middleCoord n
      where
        -- For odd-sized matrices, add the final center coordinate
        middleCoord n' = if odd n' then [(n' `div` 2, n' `div` 2)] else []

    -- Generates the coordinates for a single outer layer of the spiral
    getLayerCoords :: Int -> Int -> [(Int, Int)]
    getLayerCoords n layer =
      let top = layer
          bottom = n - 1 - layer
          left = layer
          right = n - 1 - layer
          -- Top row, left to right
          topRow = [(top, c) | c <- [left..right]]
          -- Right column, top+1 to bottom
          rightCol = [(r, right) | r <- [top+1..bottom]]
          -- Bottom row, right-1 to left (if top and bottom are different)
          bottomRow = if top /= bottom then [(bottom, c) | c <- [right-1, right-2..left]] else []
          -- Left column, bottom-1 to top+1 (if left and right are different)
          leftCol = if left /= right then [(r, left) | r <- [bottom-1, bottom-2..top+1]] else []
      in topRow ++ rightCol ++ bottomRow ++ leftCol

    -- Builds the final matrix from the list of coordinates and their corresponding values
    buildMatrix :: Int -> [(Int, Int)] -> [[Int]]
    buildMatrix n cs = [[val (i, j) | j <- [0..n-1]] | i <- [0..n-1]]
      where
        -- Create a lookup table mapping coordinates to their sequential values
        lookupTable = zip cs [1..]
        -- Function to find the value for a given coordinate
        val (i, j) = case lookup (i, j) lookupTable of
                       Just v -> v
                       -- This case should not be reached with correct coordinate generation
                       Nothing -> 0
