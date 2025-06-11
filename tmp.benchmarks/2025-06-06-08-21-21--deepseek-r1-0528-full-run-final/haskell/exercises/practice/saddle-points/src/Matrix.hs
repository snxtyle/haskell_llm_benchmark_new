module Matrix (saddlePoints) where

import Data.Array (Array, Ix, array, bounds, range, (!))

saddlePoints :: (Ix a, Ix b, Ord e) => Array (a,b) e -> [(a,b)]
saddlePoints matrix = 
  let ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
      rowIndices = range (minRow, maxRow)
      colIndices = range (minCol, maxCol)
      rowMaxima = [ (row, maximum [ matrix ! (row, col) | col <- colIndices ]) | row <- rowIndices ]
      colMinima = [ (col, minimum [ matrix ! (row, col) | row <- rowIndices ]) | col <- colIndices ]
      rowMaxArr = array (minRow, maxRow) rowMaxima
      colMinArr = array (minCol, maxCol) colMinima
  in [ (row, col) | row <- rowIndices, col <- colIndices, 
         let val = matrix ! (row, col),
         val == rowMaxArr ! row,
         val == colMinArr ! col ]
