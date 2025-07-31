module Matrix (saddlePoints) where

import Data.Array
  ( Array
  , Ix
  , bounds
  , (!) 
  )
import qualified Data.Array as A

-- | Find all indices (row, col) such that the element is the maximum
-- in its row and the minimum in its column.
-- The matrix is expected to be a 2D array with indices of the form (row, col).
saddlePoints :: (Ix i, Ix j, Ord e) => Array (i, j) e -> [(i, j)]
saddlePoints matrix =
  [ (r, c)
  | r <- A.range (rLo, rHi)
  , c <- A.range (cLo, cHi)
  , let x = matrix ! (r, c)
  , x == rowMax r
  , x == colMin c
  ]
  where
    ((rLo, cLo), (rHi, cHi)) = bounds matrix

    -- Precompute maximum of each row and minimum of each column
    rowMax r = maximum [ matrix ! (r, c) | c <- A.range (cLo, cHi) ]
    colMin c = minimum [ matrix ! (r, c) | r <- A.range (rLo, rHi) ]
