module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid = [[nextState row col | col <- [0..width-1]] | row <- [0..height-1]]
  where
    height = length grid
    width = if height > 0 then length (head grid) else 0
    
    -- Get the value at a specific position, returning 0 if out of bounds
    getCell :: Int -> Int -> Int
    getCell r c
      | r < 0 || r >= height || c < 0 || c >= width = 0
      | otherwise = (grid !! r) !! c
    
    -- Count live neighbors for a cell at position (row, col)
    countNeighbors :: Int -> Int -> Int
    countNeighbors row col = sum
      [ getCell (row + dr) (col + dc)
      | dr <- [-1, 0, 1]
      , dc <- [-1, 0, 1]
      , not (dr == 0 && dc == 0)  -- Exclude the cell itself
      ]
    
    -- Determine the next state of a cell based on Game of Life rules
    nextState :: Int -> Int -> Int
    nextState row col =
      let current = getCell row col
          neighbors = countNeighbors row col
      in case (current, neighbors) of
        (1, 2) -> 1  -- Live cell with 2 neighbors lives on
        (1, 3) -> 1  -- Live cell with 3 neighbors lives on
        (0, 3) -> 1  -- Dead cell with exactly 3 neighbors becomes alive
        _      -> 0  -- All other cells die or stay dead
