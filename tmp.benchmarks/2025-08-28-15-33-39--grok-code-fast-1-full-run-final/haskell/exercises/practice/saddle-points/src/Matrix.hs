module Matrix (saddlePoints) where

import Data.Array

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix =
  let ((rMin, cMin), (rMax, cMax)) = bounds matrix
      rowIndices r = [(r, c) | c <- [cMin..cMax]]
      colIndices c = [(r, c) | r <- [rMin..rMax]]
      isSaddle (r, c) =
        let val = matrix ! (r, c)
            rowVals = [matrix ! idx | idx <- rowIndices r]
            colVals = [matrix ! idx | idx <- colIndices c]
            rowMax = maximum rowVals
            colMin = minimum colVals
        in val == rowMax && val == colMin
  in filter isSaddle (indices matrix)
