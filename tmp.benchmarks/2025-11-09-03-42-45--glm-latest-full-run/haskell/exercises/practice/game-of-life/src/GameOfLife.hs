module GameOfLife (tick) where

-- | Safely gets the value of a cell at a given (row, col) coordinate.
-- Returns 0 if the coordinates are out of the grid's bounds.
getCell :: [[Int]] -> (Int, Int) -> Int
getCell grid (r, c)
  | r < 0 || r >= numRows || c < 0 || c >= numCols = 0
  | otherwise = grid !! r !! c
  where
    numRows = length grid
    numCols = if null grid then 0 else length (head grid)

-- | Counts the number of live neighbors for a cell at a given (row, col) coordinate.
countLiveNeighbors :: [[Int]] -> (Int, Int) -> Int
countLiveNeighbors grid (r, c) = sum $ map (getCell grid) neighborCoords
  where
    neighborCoords = [ (r', c')
                     | r' <- [r-1, r, r+1]
                     , c' <- [c-1, c, c+1]
                     , (r', c') /= (r, c) -- Exclude the cell itself
                     ]

-- | Calculates the next generation of the Game of Life grid.
tick :: [[Int]] -> [[Int]]
tick grid =
  [ [ nextCell r c | c <- [0..numCols-1] ] | r <- [0..numRows-1] ]
  where
    numRows = length grid
    numCols = if null grid then 0 else length (head grid)

    -- Determines the next state of a cell at (r, c) based on the rules.
    nextCell :: Int -> Int -> Int
    nextCell r c =
      let liveNeighbors = countLiveNeighbors grid (r, c)
          currentCell = getCell grid (r, c)
      in if currentCell == 1
         then if liveNeighbors == 2 || liveNeighbors == 3 then 1 else 0
         else if liveNeighbors == 3 then 1 else 0
