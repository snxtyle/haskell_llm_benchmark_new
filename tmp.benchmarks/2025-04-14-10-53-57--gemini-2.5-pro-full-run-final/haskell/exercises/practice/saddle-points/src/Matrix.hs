module Matrix (saddlePoints) where

import Data.Array (Array, bounds, indices, (!))
import Data.List (maximum, minimum)

-- | Find saddle points in a 2D array.
-- A saddle point is an element that is greater than or equal to
-- every element in its row and less than or equal to every element
-- in its column.
saddlePoints :: Ord e => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix
  -- Check if the array has any elements. If not, return empty list.
  -- indices returns [] for an array with invalid bounds (e.g., min > max).
  | null idxs = []
  | otherwise =
      -- Filter the indices to keep only those that are saddle points.
      filter isSaddle idxs
  where
    -- Get all valid indices of the array.
    idxs = indices matrix
    -- Get the bounds ((minRow, minCol), (maxRow, maxCol)).
    ((minR, minC), (maxR, maxC)) = bounds matrix

    -- Check if the element at the given index is the maximum in its row.
    isRowMax :: (Int, Int) -> Bool
    isRowMax (r, c) =
      let val = matrix ! (r, c)
          -- Get all values in the specified row 'r'.
          rowVals = [ matrix ! (r, col) | col <- [minC..maxC] ]
      -- Check if the element's value is the maximum in the row.
      -- maximum is safe here because rows in a valid Array are non-empty.
      in val == maximum rowVals

    -- Check if the element at the given index is the minimum in its column.
    isColMin :: (Int, Int) -> Bool
    isColMin (r, c) =
      let val = matrix ! (r, c)
          -- Get all values in the specified column 'c'.
          colVals = [ matrix ! (row, c) | row <- [minR..maxR] ]
      -- Check if the element's value is the minimum in the column.
      -- minimum is safe here because columns in a valid Array are non-empty.
      in val == minimum colVals

    -- Check if the element at the given index is both a row maximum and a column minimum.
    isSaddle :: (Int, Int) -> Bool
    isSaddle idx = isRowMax idx && isColMin idx
