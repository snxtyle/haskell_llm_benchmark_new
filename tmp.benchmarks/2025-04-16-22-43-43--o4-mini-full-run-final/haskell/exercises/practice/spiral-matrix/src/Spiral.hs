module Spiral (spiral) where

import qualified Data.Array as A

-- | Generate an n×n matrix filled in a clockwise inward spiral with 1..n*n.
spiral :: Int -> [[Int]]
spiral n
  | n <= 0    = []
  | otherwise =
      let bounds    = ((0,0), (n-1,n-1))
          positions = spiralCoords n
          arr       = A.array bounds (zip positions [1 .. n*n])
      in [ [ arr A.! (i,j) | j <- [0 .. n-1] ]
         | i <- [0 .. n-1]
         ]

-- | Produce the sequence of (row,column) positions in spiral order.
spiralCoords :: Int -> [(Int,Int)]
spiralCoords n = go 0 n
  where
    go offset size
      | size <= 0 = []
      | size == 1 = [(offset, offset)]
      | otherwise =
          let x0 = offset
              y0 = offset
              x1 = offset + size - 1
              y1 = offset + size - 1

              top    = [ (x0, y)     | y <- [y0 .. y1] ]
              right  = [ (x,  y1)    | x <- [x0+1 .. x1] ]
              bottom = [ (x1, y)     | y <- [y1-1, y1-2 .. y0] ]
              left   = [ (x,  y0)    | x <- [x1-1, x1-2 .. x0+1] ]
          in  top ++ right ++ bottom ++ left
             ++ go (offset+1) (size-2)
