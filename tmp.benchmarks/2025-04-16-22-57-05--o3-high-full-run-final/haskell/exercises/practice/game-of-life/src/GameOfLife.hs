module GameOfLife (tick) where

-- | Advance the Game of Life grid by one generation.
--   0 represents a dead cell, 1 represents a live cell.
tick :: [[Int]] -> [[Int]]
tick grid = [ [ nextState i j | j <- [0 .. width i - 1] ] | i <- [0 .. height - 1] ]
  where
    height = length grid
    width i = length (rowAt i)

    -- Retrieve the row at index i if it exists, otherwise the empty list.
    rowAt :: Int -> [Int]
    rowAt idx
      | idx < 0 || idx >= height = []
      | otherwise                = grid !! idx

    -- Safe cell lookup; returns 0 for out-of-bounds coordinates.
    cell :: Int -> Int -> Int
    cell i j
      | i < 0 || j < 0          = 0
      | i >= height             = 0
      | j >= length row         = 0
      | otherwise               = row !! j
      where
        row = rowAt i

    -- Count the number of live neighbours for grid cell (i, j).
    liveNeighbors :: Int -> Int -> Int
    liveNeighbors i j =
      sum [ cell (i + di) (j + dj)
          | di <- [-1, 0, 1]
          , dj <- [-1, 0, 1]
          , not (di == 0 && dj == 0)
          ]

    -- Determine the next state (0 or 1) for cell (i, j).
    nextState :: Int -> Int -> Int
    nextState i j =
      let current    = cell i j
          neighbours = liveNeighbors i j
      in case current of
           1 -> if neighbours == 2 || neighbours == 3 then 1 else 0
           0 -> if neighbours == 3               then 1 else 0
           _ -> 0  -- Treat any non‑0/1 value as dead.
