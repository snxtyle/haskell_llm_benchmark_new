module GameOfLife (tick) where

-- | Compute the next generation of Conway's Game of Life
--   for a rectangular (or ragged) grid represented as a
--   list of rows.  A value of @1@ denotes a live cell,
--   @0@ denotes a dead cell.  Any indices that fall
--   outside the provided grid are considered dead.
tick :: [[Int]] -> [[Int]]
tick grid = [ [ nextState y x | x <- [0 .. width - 1] ]
            | y <- [0 .. height - 1]
            ]
  where
    height = length grid
    width  = maximum (0 : map length grid)

    -- | Determine the next state (0 or 1) of the cell at (y, x).
    nextState :: Int -> Int -> Int
    nextState y x =
      case current of
        1 | liveN == 2 || liveN == 3 -> 1
        0 | liveN == 3               -> 1
        _                            -> 0
      where
        current = cellAt y x
        liveN   = liveNeighbors y x

    -- | Count the number of live neighbours around a cell.
    liveNeighbors :: Int -> Int -> Int
    liveNeighbors y x =
      sum
        [ cellAt (y + dy) (x + dx)
        | dy <- [-1, 0, 1]
        , dx <- [-1, 0, 1]
        , (dy, dx) /= (0, 0)
        ]

    -- | Safely fetch the value at (r, c); returns 0 for any out‑of‑bounds access.
    cellAt :: Int -> Int -> Int
    cellAt r c =
      case safeIndex grid r of
        Just row -> case safeIndex row c of
                      Just val -> val
                      Nothing  -> 0
        Nothing  -> 0

-- | Safe list indexing that returns Nothing if the index is out of bounds.
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0         = Nothing
  | i >= length xs = Nothing
  | otherwise     = Just (xs !! i)
