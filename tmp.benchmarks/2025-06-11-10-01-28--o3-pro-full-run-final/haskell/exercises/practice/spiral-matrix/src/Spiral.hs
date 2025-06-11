module Spiral (spiral) where

import Data.Array (Array, array, (!))

-- | Produce an n × n matrix filled with consecutive natural numbers
--   arranged in a clockwise spiral starting at the top-left corner.
--
--   Examples:
--
--   >>> spiral 3
--   [[1,2,3],
--    [8,9,4],
--    [7,6,5]]
--
--   >>> spiral 4
--   [[1,2,3,4],
--    [12,13,14,5],
--    [11,16,15,6],
--    [10,9,8,7]]
--
--   If the supplied size is not positive, the result is the empty list.
spiral :: Int -> [[Int]]
spiral n
  | n <= 0    = []
  | otherwise = [ [ grid ! (r, c) | c <- [0 .. n - 1] ]
                | r <- [0 .. n - 1]
                ]
  where
    -- Array that maps each coordinate to its corresponding value
    grid :: Array (Int, Int) Int
    grid = array ((0, 0), (n - 1, n - 1)) (zip (spiralCoords n) [1 ..])

--------------------------------------------------------------------------------
-- Internal helpers

-- | Generate the list of matrix coordinates in the required spiral order.
spiralCoords :: Int -> [(Int, Int)]
spiralCoords size = concatMap layer [0 .. layers - 1]
  where
    layers = (size + 1) `div` 2

    layer :: Int -> [(Int, Int)]
    layer offset =
      let m = size - 2 * offset            -- current layer’s side length
          top    = [ (offset,            c) | c <- [offset .. offset + m - 1] ]
          right  = [ (r, offset + m - 1)    | r <- [offset + 1 .. offset + m - 1] ]
          bottom = if m > 1
                     then [ (offset + m - 1, c)
                          | c <- reverse [offset .. offset + m - 2] ]
                     else []
          left   = if m > 1
                     then [ (r, offset) 
                          | r <- reverse [offset + 1 .. offset + m - 2] ]
                     else []
      in top ++ right ++ bottom ++ left
