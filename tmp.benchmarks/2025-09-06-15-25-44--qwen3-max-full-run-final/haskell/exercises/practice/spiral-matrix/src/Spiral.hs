module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral 0 = []
spiral n = spiralHelper n 1
  where
    spiralHelper 0 _ = []
    spiralHelper 1 start = [[start]]
    spiralHelper size start =
      let topRow = [start .. start + size - 1]
          rightCol = [start + size .. start + 2 * size - 2]
          bottomRow = reverse [start + 2 * size - 1 .. start + 3 * size - 3]
          leftCol = reverse [start + 3 * size - 2 .. start + 4 * size - 4]
          innerSize = size - 2
          innerMatrix = if innerSize > 0 then spiralHelper innerSize (start + 4 * size - 4) else []
          -- Build the middle rows by combining left column, inner matrix, and right column
          middleRows = if innerSize > 0
                       then zipWith3 (\left inner right -> [left] ++ inner ++ [right]) leftCol innerMatrix rightCol
                       else if size == 2
                            then []  -- Special case for size 2: no middle rows, just top and bottom
                            else zipWith (\left right -> [left, right]) leftCol rightCol
      in if size == 2
         then [topRow, bottomRow]
         else [topRow] ++ middleRows ++ [bottomRow]
