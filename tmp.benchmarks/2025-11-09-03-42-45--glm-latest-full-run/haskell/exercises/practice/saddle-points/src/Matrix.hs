module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!), IArray)
import Data.Ix (Ix)
import Data.List (maximum, minimum)

-- | Finds all saddle points in a matrix.
-- A saddle point is an element that is the maximum in its row
-- and the minimum in its column.
--
-- The function assumes a 2D array, so the index type `i` is (Int, Int).
-- The element type `e` must be orderable (in the `Ord` typeclass).
saddlePoints :: (Ix i, IArray Array e, Ord e) => Array i e -> [i]
saddlePoints matrix =
  let -- Get the bounds of the 2D array. We assume the index is a tuple (row, col).
      ((minRow, minCol), (maxRow, maxCol)) = bounds matrix

      -- Generate all possible indices in the matrix.
      allIndices = [(r, c) | r <- [minRow..maxRow], c <- [minCol..maxCol]]

      -- Helper function to check if a single index is a saddle point.
      isSaddlePoint (r, c) =
        let value = matrix ! (r, c)

            -- Get all values in the current row 'r'.
            rowValues = [matrix ! (r, col) | col <- [minCol..maxCol]]
            -- Get all values in the current column 'c'.
            colValues = [matrix ! (row, c) | row <- [minRow..maxRow]]

        -- A saddle point is the max in its row AND the min in its column.
        in value == maximum rowValues && value == minimum colValues

  -- Filter all indices, keeping only those that satisfy the saddle point condition.
  in filter isSaddlePoint allIndices
