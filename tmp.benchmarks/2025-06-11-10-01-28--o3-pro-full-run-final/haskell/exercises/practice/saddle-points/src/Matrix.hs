module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!))
import Data.Ix (Ix, range)

-- | Find all saddle points in a given 2-dimensional Array.
--   A saddle point is an element that is the maximum in its row
--   and simultaneously the minimum in its column.
--
--   The indices are assumed to be a pair @(row, column)@.
--
--   The function works for any index types that are instances of 'Ix'
--   and for any element type that has an 'Ord' instance.
saddlePoints :: (Ix i, Ix j, Ord a) => Array (i, j) a -> [(i, j)]
saddlePoints matrix
  | null allIndices = []
  | otherwise =
      [ (r, c)
      | r <- rows
      , c <- cols
      , let v = matrix ! (r, c)
      , v == rowMax r
      , v == colMin c
      ]
  where
    -- Bounds and index ranges
    ((rowMinIdx, colMinIdx), (rowMaxIdx, colMaxIdx)) = bounds matrix
    rows       = range (rowMinIdx, rowMaxIdx)
    cols       = range (colMinIdx, colMaxIdx)
    allIndices = range ((rowMinIdx, colMinIdx), (rowMaxIdx, colMaxIdx))

    -- Pre-compute maximum of each row
    rowMaxMap =
      [ (r, maximum [ matrix ! (r, c) | c <- cols ])
      | r <- rows
      ]

    -- Pre-compute minimum of each column
    colMinMap =
      [ (c, minimum [ matrix ! (r, c) | r <- rows ])
      | c <- cols
      ]

    -- Helper functions to fetch pre-computed values
    rowMax r = fetch r rowMaxMap
    colMin c = fetch c colMinMap

    fetch key = maybe (error "Internal error: key not found") id . lookup key
