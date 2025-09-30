module Matrix (saddlePoints) where

import Data.Array (Array, Ix, bounds, indices, (!), assocs)
import Data.List (nub)

saddlePoints :: (Ix i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix
  | null (assocs matrix) = []
  | otherwise = filter isSaddlePoint (indices matrix)
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
    
    isSaddlePoint (r, c) =
      let val = matrix ! (r, c)
          isMaxInRow = all (\col -> matrix ! (r, col) <= val) [minCol..maxCol]
          isMinInCol = all (\row -> matrix ! (row, c) >= val) [minRow..maxRow]
      in isMaxInRow && isMinInCol
