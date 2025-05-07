module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!))
import Data.Ix (Ix)

saddlePoints :: (Ix row, Ix col, Ord e)
             => Array (row, col) e -> [(row, col)]
saddlePoints arr =
  let ((rMin, cMin), (rMax, cMax)) = bounds arr
      rows = [rMin .. rMax]
      cols = [cMin .. cMax]
      -- maximum value in a given row
      rowMax r = maximum [ arr ! (r, c) | c <- cols ]
      -- minimum value in a given column
      colMin c = minimum [ arr ! (r, c) | r <- rows ]
  in [ (r, c)
     | r <- rows
     , c <- cols
     , let v = arr ! (r, c)
     , v == rowMax r
     , v == colMin c
     ]
