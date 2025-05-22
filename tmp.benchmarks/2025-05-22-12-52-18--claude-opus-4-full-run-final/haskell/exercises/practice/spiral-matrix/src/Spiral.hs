module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral 0 = []
spiral n = [[getValueAt row col | col <- [0..n-1]] | row <- [0..n-1]]
  where
    getValueAt :: Int -> Int -> Int
    getValueAt row col = 
      let layer = minimum [row, col, n - 1 - row, n - 1 - col]
          -- Calculate the starting value for this layer
          prevLayersCount = if layer == 0 then 0 else 4 * layer * (n - layer)
          -- Position within the current layer
          posInLayer = getPositionInLayer row col layer
      in prevLayersCount + posInLayer + 1
    
    getPositionInLayer :: Int -> Int -> Int -> Int
    getPositionInLayer row col layer
      -- Top edge (moving right)
      | row == layer && col >= layer && col < n - layer - 1 = 
          col - layer
      -- Right edge (moving down)
      | col == n - layer - 1 && row >= layer && row < n - layer - 1 = 
          (n - 2 * layer - 1) + (row - layer)
      -- Bottom edge (moving left)
      | row == n - layer - 1 && col > layer && col <= n - layer - 1 = 
          2 * (n - 2 * layer - 1) + (n - layer - 1 - col)
      -- Left edge (moving up)
      | col == layer && row > layer && row <= n - layer - 1 = 
          3 * (n - 2 * layer - 1) + (n - layer - 1 - row)
      -- Corner case: when we're at the center of an odd-sized matrix
      | otherwise = 0
