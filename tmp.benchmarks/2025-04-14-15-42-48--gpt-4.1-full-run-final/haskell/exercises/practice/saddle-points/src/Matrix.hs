module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!), indices)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Ix (Ix, range)

-- | Returns the list of indices that are saddle points in the matrix.
--   A saddle point is the largest in its row and the smallest in its column.
saddlePoints :: (Ix i, Ix j, Ord e) => Array (i, j) e -> [(i, j)]
saddlePoints matrix =
    [ (i, j)
    | (i, j) <- indices matrix
    , let rowVals = [matrix ! (i, y) | y <- js]
    , let colVals = [matrix ! (x, j) | x <- is]
    , matrix ! (i, j) == maximum rowVals
    , matrix ! (i, j) == minimum colVals
    ]
  where
    ((iMin, jMin), (iMax, jMax)) = bounds matrix
    is = range (iMin, iMax)
    js = range (jMin, jMax)
