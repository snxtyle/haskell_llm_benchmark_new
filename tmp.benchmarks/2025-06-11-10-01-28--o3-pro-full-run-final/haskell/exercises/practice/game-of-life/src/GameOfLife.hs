module GameOfLife (tick) where

-- | Compute the next generation for Conway's Game of Life.
--   The board is represented as a matrix of Ints where
--   1 = alive, 0 = dead.
--
--   Example:
--   >>> tick [[0,1,0],
--   ...       [0,1,0],
--   ...       [0,1,0]]
--   [[0,0,0],
--    [1,1,1],
--    [0,0,0]]
tick :: [[Int]] -> [[Int]]
tick grid
  | null grid || null (head grid) = grid -- Empty board, nothing to do
  | otherwise                     = [[ nextState r c | c <- [0 .. cols - 1]]
                                                   | r <- [0 .. rows - 1]]
  where
    rows = length grid
    cols = length (head grid)

    -- Safely get a cell; outside the board is considered dead.
    cell :: Int -> Int -> Int
    cell r c
      | r < 0 || c < 0 || r >= rows || c >= cols = 0
      | otherwise = (grid !! r) !! c

    -- Count live neighbours for a given position.
    liveNeighbours :: Int -> Int -> Int
    liveNeighbours r c =
      sum [ cell (r + dr) (c + dc)
          | dr <- [-1 .. 1]
          , dc <- [-1 .. 1]
          , not (dr == 0 && dc == 0)
          ]

    -- Determine the next state of a cell following Conway's rules.
    nextState :: Int -> Int -> Int
    nextState r c =
      case (current, liveNeighbours r c) of
        (1, n) | n == 2 || n == 3 -> 1  -- Survival
        (0, 3)                    -> 1  -- Birth
        _                         -> 0  -- Death / Stasis
      where
        current = cell r c
