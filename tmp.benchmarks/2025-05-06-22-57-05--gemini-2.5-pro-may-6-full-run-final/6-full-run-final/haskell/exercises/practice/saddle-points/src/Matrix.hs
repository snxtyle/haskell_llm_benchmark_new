module Matrix (saddlePoints) where

import Data.Array (Array, bounds, indices, (!), Ix)
-- Ix is imported for completeness, as (Int, Int) must be an instance of Ix.
-- It's often not strictly necessary to import Ix explicitly when using concrete index types
-- like (Int,Int) as the Array type constructor already implies it for its index type.

-- The type signature is specialized to handle 2D arrays where indices are pairs of Ints,
-- representing (row, column), and elements are of a type that can be ordered (Ord e).
saddlePoints :: (Ord e) => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix
  -- If the list of indices is empty, the matrix has no elements, so no saddle points.
  -- This handles matrices with zero elements (e.g., bounds ((1,1),(0,0)) where minIndex > maxIndex).
  | null idxList = []
  | otherwise =
    [ (r, c) | (r, c) <- idxList,          -- Iterate over each index (r, c) in the matrix
               let val = matrix ! (r, c),  -- Get the element at that index
               isMaxInRow r val,           -- Check if it's the largest in its row
               isMinInCol c val            -- Check if it's the smallest in its column
    ]
  where
    -- Get all valid indices of the matrix. For an empty matrix (e.g. bounds ((1,1),(0,0))), this will be [].
    idxList = indices matrix

    -- Determine the range of rows and columns.
    -- This is safe because if idxList is non-empty, `bounds matrix` is well-defined,
    -- and minR <= maxR, minC <= maxC.
    ((minR, minC), (maxR, maxC)) = bounds matrix

    -- Helper function to check if 'val' is the largest element in row 'r'.
    -- Its type will be inferred correctly by the compiler as:
    -- isMaxInRow :: Int -> e -> Bool
    -- where 'e' is the same 'e' from saddlePoints's signature.
    isMaxInRow r val =
      all (\currentC -> val >= matrix ! (r, currentC)) [minC..maxC]

    -- Helper function to check if 'val' is the smallest element in column 'c'.
    -- Its type will be inferred correctly by the compiler as:
    -- isMinInCol :: Int -> e -> Bool
    -- where 'e' is the same 'e' from saddlePoints's signature.
    isMinInCol c val =
      all (\currentR -> val <= matrix ! (currentR, c)) [minR..maxR]

