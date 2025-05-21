module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid =
  let
    rows = length grid
    cols = if rows > 0 then length (head grid) else 0

    -- Helper to get cell value safely, returning 0 for out-of-bounds
    getCellValue r c
      | r < 0 || r >= rows || c < 0 || c >= cols = 0
      | otherwise = (grid !! r) !! c

    -- Helper to count live neighbors for a cell at (r, c)
    countLiveNeighbors r c =
      sum [ getCellValue (r + dr) (c + dc)
          | dr <- [-1, 0, 1]
          , dc <- [-1, 0, 1]
          , (dr, dc) /= (0, 0) -- Exclude the cell itself
          ]

    -- Helper to determine the next state of a cell based on Game of Life rules
    nextCellState currentState liveNeighbors
      | currentState == 1 = -- Live cell
          if liveNeighbors == 2 || liveNeighbors == 3
            then 1 -- Lives on
            else 0 -- Dies (underpopulation or overpopulation)
      | currentState == 0 = -- Dead cell
          if liveNeighbors == 3
            then 1 -- Becomes alive (reproduction)
            else 0 -- Stays dead
      | otherwise = error "Invalid cell state (must be 0 or 1)" -- Should not happen with valid input
  in
    -- Handle empty grid or grid with empty rows
    if rows == 0 || cols == 0
      then grid
      else
        -- Build the new grid using list comprehensions
        [ [ nextCellState (getCellValue r c) (countLiveNeighbors r c)
          | c <- [0 .. cols - 1]
          ]
        | r <- [0 .. rows - 1]
        ]
