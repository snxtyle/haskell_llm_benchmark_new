module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n = go n 1
  where
    go 0 _ = []
    go 1 start = [[start]]
    go size start =
      let
        -- Top row
        top = [start .. start + size - 1]
        -- Right column (excluding the top cell)
        right = [start + size .. start + 2 * size - 2]
        -- Bottom row (excluding the rightmost cell), reversed
        bottom = reverse [start + 2 * size - 1 .. start + 3 * size - 3]
        -- Left column (excluding the top and bottom cells), reversed
        left = reverse [start + 3 * size - 2 .. start + 4 * size - 5]
        -- Recursively build the inner spiral
        inner = go (size - 2) (start + 4 * size - 4)
        -- Combine all sides into the matrix
        -- The first row is 'top'
        -- The last row is 'bottom'
        -- The middle rows are: left (from top to bottom), inner rows, right (from top to bottom)
        -- For each middle row, take one from left, one from inner, one from right
        middleRows =
          if null inner
            then []
            else zipWith3 (\l inn r -> [l] ++ inn ++ [r]) left inner right
      in
        [top]
        ++ middleRows
        ++ [bottom | size > 1]
