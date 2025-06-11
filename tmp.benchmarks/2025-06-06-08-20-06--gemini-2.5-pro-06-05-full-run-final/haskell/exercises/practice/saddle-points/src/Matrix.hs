module Matrix (saddlePoints) where

import Data.Array (Array, Ix, (!), array, bounds, indices, range)

saddlePoints :: (Ord e, Ix r, Ix c) => Array (r, c) e -> [(r, c)]
saddlePoints matrix
  | null (indices matrix) = []
  | otherwise = filter isSaddlePoint (indices matrix)
  where
    ((minR, minC), (maxR, maxC)) = bounds matrix

    rowMaxs =
      array (minR, maxR)
        [(r, maximum [matrix ! (r, j) | j <- range (minC, maxC)]) | r <- range (minR, maxR)]

    colMins =
      array (minC, maxC)
        [(c, minimum [matrix ! (i, c) | i <- range (minR, maxR)]) | c <- range (minC, maxC)]

    isSaddlePoint (r, c) =
      let val = matrix ! (r, c)
      in val == rowMaxs ! r && val == colMins ! c
