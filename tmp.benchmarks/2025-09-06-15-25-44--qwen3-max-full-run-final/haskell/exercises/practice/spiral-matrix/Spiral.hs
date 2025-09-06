module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size = spiralHelper size 1
  where
    spiralHelper 0 _ = []
    spiralHelper 1 start = [[start]]
    spiralHelper n start = 
      let topRow = [start .. start + n - 1]
          rightCol = [start + n .. start + 2 * n - 2]
          bottomRow = reverse [start + 2 * n - 1 .. start + 3 * n - 3]
          leftCol = reverse [start + 3 * n - 2 .. start + 4 * n - 4]
          innerMatrix = spiralHelper (n - 2) (start + 4 * n - 4)
          withRightCol = zipWith (\row val -> row ++ [val]) innerMatrix rightCol
          withLeftCol = zipWith (\val row -> [val] ++ row) leftCol withRightCol
      in [topRow] ++ withLeftCol ++ [bottomRow]
