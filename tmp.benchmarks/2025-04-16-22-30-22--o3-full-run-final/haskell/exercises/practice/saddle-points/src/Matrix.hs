module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!))
import Data.Ix (range)

-- | Find all saddle points in the given matrix.
--   A saddle point is an element which is the largest in its row
--   and simultaneously the smallest in its column.
--
--   The indices are one‑based when the matrix is built the same way
--   the Exercism tests do (using 1 as the first row and column index).
--
--   If the matrix is empty (has no rows or no columns) the result is [].
saddlePoints :: (Ord a) => Array (Int, Int) a -> [(Int, Int)]
saddlePoints matrix
  | null rows || null cols = []
  | otherwise              =
      [ (r, c)
      | r <- rows
      , c <- cols
      , let val       = matrix ! (r, c)
            maxInRow  = maximum [ matrix ! (r, c') | c' <- cols ]
            minInCol  = minimum [ matrix ! (r', c) | r' <- rows ]
      , val == maxInRow && val == minInCol
      ]
  where
    ((rMin, cMin), (rMax, cMax)) = bounds matrix
    rows = range (rMin, rMax)
    cols = range (cMin, cMax)
