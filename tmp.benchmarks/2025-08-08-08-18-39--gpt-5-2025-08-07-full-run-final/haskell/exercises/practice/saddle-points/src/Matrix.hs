module Matrix (saddlePoints) where

import Data.Array (Array, Ix, bounds, (!), range)

-- | Find all indices (row, column) that are the largest in their row
--   and the smallest in their column.
saddlePoints :: (Ix r, Ix c, Ord e) => Array (r, c) e -> [(r, c)]
saddlePoints matrix =
  [ (r, c)
  | r <- range (rMin, rMax)
  , c <- range (cMin, cMax)
  , let x = matrix ! (r, c)
  , x == rowMax r
  , x == colMin c
  ]
  where
    ((rMin, cMin), (rMax, cMax)) = bounds matrix

    rowMax r = maximum [ matrix ! (r, c) | c <- range (cMin, cMax) ]
    colMin c = minimum [ matrix ! (r, c) | r <- range (rMin, rMax) ]
