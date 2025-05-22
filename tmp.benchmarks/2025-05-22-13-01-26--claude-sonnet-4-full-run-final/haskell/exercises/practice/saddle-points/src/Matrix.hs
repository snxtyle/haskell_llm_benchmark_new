module Matrix (saddlePoints) where

import Data.Array (Array, bounds, indices, (!), elems)
import Data.Ix (Ix)

saddlePoints :: (Ix i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = filter isSaddlePoint (indices matrix)
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
    
    isSaddlePoint pos@(r, c) = isRowMax pos && isColMin pos
    
    isRowMax (r, c) = 
      let rowValues = [matrix ! (r, col) | col <- [minCol..maxCol]]
          currentValue = matrix ! (r, c)
      in all (<= currentValue) rowValues
    
    isColMin (r, c) = 
      let colValues = [matrix ! (row, c) | row <- [minRow..maxRow]]
          currentValue = matrix ! (r, c)
      in all (>= currentValue) colValues
