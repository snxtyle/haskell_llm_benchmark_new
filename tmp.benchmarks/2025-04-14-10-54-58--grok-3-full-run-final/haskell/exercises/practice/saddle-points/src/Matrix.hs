module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!), indices)
import Data.Ix (Ix)

saddlePoints :: (Ix i, Ord e) => Array i e -> [i]
saddlePoints matrix = filter isSaddlePoint (indices matrix)
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
    
    -- Get all values in a row
    rowValues row = [matrix ! (row, col) | col <- [minCol..maxCol]]
    
    -- Get all values in a column
    colValues col = [matrix ! (row, col) | row <- [minRow..maxRow]]
    
    -- Check if a point is a saddle point
    isSaddlePoint (row, col) = 
        let value = matrix ! (row, col)
            rowVals = rowValues row
            colVals = colValues col
        in value == maximum rowVals && value == minimum colVals
