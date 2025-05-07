module GameOfLife (tick) where

-- | Calculate the next generation of the Game of Life grid.
tick :: [[Int]] -> [[Int]]
tick grid
  | null grid || null (head grid) = [] -- Handle empty grid
  | otherwise = [[nextCellState r c | c <- [0..cols-1]] | r <- [0..rows-1]]
  where
    rows = length grid
    cols = length (head grid)

    -- Safely get the value of a cell, returning 0 for out-of-bounds coordinates.
    getCellValue :: Int -> Int -> Int
    getCellValue r c
      | r < 0 || r >= rows || c < 0 || c >= cols = 0
      | otherwise = grid !! r !! c

    -- Count live neighbors for a cell at (r, c).
    countLiveNeighbors :: Int -> Int -> Int
    countLiveNeighbors r c = sum
      [ getCellValue nr nc
      | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0)
      , let nr = r + dr
      , let nc = c + dc
      ]

    -- Determine the next state of a cell based on rules.
    nextCellState :: Int -> Int -> Int
    nextCellState r c =
      let currentState = getCellValue r c
          liveNeighbors = countLiveNeighbors r c
      in case (currentState, liveNeighbors) of
           (1, 2) -> 1 -- Rule 1: Live cell with 2 live neighbors lives on.
           (1, 3) -> 1 -- Rule 1: Live cell with 3 live neighbors lives on.
           (0, 3) -> 1 -- Rule 2: Dead cell with 3 live neighbors becomes alive.
           _      -> 0 -- Rule 3: All other cells die or stay dead.
