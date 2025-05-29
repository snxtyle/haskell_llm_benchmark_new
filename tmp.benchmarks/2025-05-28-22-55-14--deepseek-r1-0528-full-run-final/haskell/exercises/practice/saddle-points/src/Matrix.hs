module Matrix (saddlePoints) where

import Data.Array (Array, bounds, Ix, (!))
import Data.List (elemIndices)

saddlePoints :: (Ix i, Ord e, Num i, Enum i) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = 
  let ((minR, minC), (maxR, maxC)) = bounds matrix
      rowIndices = [minR..maxR]
      colIndices = [minC..maxC]
      rowMaxs = [ maximum [ matrix ! (i, j) | j <- colIndices ] | i <- rowIndices ]
      colMins = [ minimum [ matrix ! (i, j) | i <- rowIndices ] | j <- colIndices ]
  in [ (i, j) | 
        i <- rowIndices, 
        j <- colIndices,
        matrix ! (i, j) == rowMaxs !! (fromEnum (i - minR)),
        matrix ! (i, j) == colMins !! (fromEnum (j - minC))
      ]
