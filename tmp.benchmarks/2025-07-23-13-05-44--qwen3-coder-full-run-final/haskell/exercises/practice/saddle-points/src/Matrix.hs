module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!))
import Data.Ix (Ix, range)

saddlePoints :: (Ix i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = 
  let ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
      rows = [minRow..maxRow]
      cols = [minCol..maxCol]
      -- For each row, find the maximum value
      rowMaxes = [maximum [matrix ! (r, c) | c <- cols] | r <- rows]
      -- For each column, find the minimum value
      colMins = [minimum [matrix ! (r, c) | r <- rows] | c <- cols]
      -- Check each position to see if it's both row max and column min
      isSaddlePoint (r, c) = 
        let value = matrix ! (r, c)
        in value == rowMaxes !! (fromEnum r - fromEnum minRow) 
           && value == colMins !! (fromEnum c - fromEnum minCol)
  in [(r, c) | r <- rows, c <- cols, isSaddlePoint (r, c)]
