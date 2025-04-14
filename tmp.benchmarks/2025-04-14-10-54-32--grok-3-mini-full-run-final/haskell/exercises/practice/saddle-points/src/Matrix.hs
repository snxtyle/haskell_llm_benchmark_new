module Matrix (saddlePoints) where

import Data.Array (Array, bounds, indices, (!), Ix)

saddlePoints :: (Ix i, Ord e) => Array i e -> [i]
saddlePoints matrix = [ idx | idx <- indices matrix, isSaddle idx matrix ]
  where
    isSaddle idx matrix =
      let val = matrix ! idx
          b@((rowMin, colMin), (rowMax, colMax)) = bounds matrix  -- Assuming 2D array with (Int, Int) indices
          (i, j) = idx  -- idx is expected to be (Int, Int)
          rowValues = [ matrix ! (i, k) | k <- [colMin .. colMax] ]
          colValues = [ matrix ! (k, j) | k <- [rowMin .. rowMax] ]
      in val == maximum rowValues && val == minimum colValues
