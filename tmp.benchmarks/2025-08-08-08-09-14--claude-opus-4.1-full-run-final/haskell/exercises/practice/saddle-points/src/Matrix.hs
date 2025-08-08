module Matrix (saddlePoints) where

import Data.Array (Array, Ix, bounds, indices, (!))
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

saddlePoints :: (Ix i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = filter isSaddlePoint (indices matrix)
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
    
    -- Get all values in a specific row
    rowValues row = [matrix ! (row, col) | col <- [minCol..maxCol]]
    
    -- Get all values in a specific column
    colValues col = [matrix ! (row, col) | row <- [minRow..maxRow]]
    
    -- Check if a position is a saddle point
    isSaddlePoint (row, col) = 
      let value = matrix ! (row, col)
          rowMax = maximum (rowValues row)
          colMin = minimum (colValues col)
      in value == rowMax && value == colMin
