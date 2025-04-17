module Spiral (spiral) where

-- | Produce an n×n matrix filled with the numbers 1 .. n²
--   in a clockwise spiral order.
--
--   For n <= 0 we return the empty matrix.
--
--   Examples (rendered row‑wise):
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
spiral :: Int -> [[Int]]
spiral n
  | n <= 0    = []
  | otherwise = [ [ valueAt (r, c) | c <- [0 .. n - 1] ]
                | r <- [0 .. n - 1]
                ]
  where
    coords     = spiralCoords n               -- [(row,col)] in spiral order
    pairs      = zip coords [1 .. n * n]      -- association list (coord ↦ value)

    valueAt :: (Int, Int) -> Int
    valueAt pos =
      case lookup pos pairs of
        Just v  -> v
        Nothing -> error "Internal error: coordinate not found"

-- Produce the list of coordinates in the required spiral order.
spiralCoords :: Int -> [(Int, Int)]
spiralCoords n = go 0 (n - 1) 0 (n - 1)
  where
    go :: Int -> Int -> Int -> Int -> [(Int, Int)]
    go top bottom left right
      | top > bottom || left > right = []
      | otherwise =
          -- move right across the top row
          [ (top, c) | c <- [left .. right] ] ++
          -- move down the rightmost column
          [ (r, right) | r <- [top + 1 .. bottom] ] ++
          -- move left across the bottom row (if any rows remain)
          (if top < bottom
             then [ (bottom, c) | c <- [right - 1, right - 2 .. left] ]
             else []) ++
          -- move up the leftmost column (if any columns remain)
          (if left < right
             then [ (r, left) | r <- [bottom - 1, bottom - 2 .. top + 1] ]
             else []) ++
          -- recurse to the inner sub‑matrix
          go (top + 1) (bottom - 1) (left + 1) (right - 1)
