module Matrix (saddlePoints) where

import Data.Array (Array, Ix, bounds, indices, (!))
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

saddlePoints :: (Ix i, Num i, Enum i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = 
  let ((rMin, cMin), (rMax, cMax)) = bounds matrix
      rows = [rMin .. rMax]
      cols = [cMin .. cMax]

      -- For each row, find the maximum value
      maxInRow r = maximum [matrix ! (r, c) | c <- cols]

      -- For each column, find the minimum value
      minInCol c = minimum [matrix ! (r, c) | r <- rows]

      -- A point is a saddle point if it is the max in its row and min in its column
      isSaddlePoint (r, c) = 
        let val = matrix ! (r, c)
        in val == maxInRow r && val == minInCol c

  in filter isSaddlePoint (indices matrix)
