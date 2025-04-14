module GameOfLife (tick) where

-- | Compute the next generation of the Game of Life grid.
tick :: [[Int]] -> [[Int]]
tick grid = mapRows 0 grid
  where
    height = length grid
    width = if height > 0 then length (head grid) else 0
    
    -- Map over rows with index
    mapRows _ [] = []
    mapRows y (row:rows) = mapRow y 0 row : mapRows (y + 1) rows
    
    -- Map over cells in a row with index
    mapRow y x row
      | x >= width = []
      | otherwise = nextState y x grid : mapRow y (x + 1) row
    
    -- Calculate the next state of a cell based on the rules of Game of Life
    nextState y x g = 
      let aliveNeighbors = countAliveNeighbors y x g
          currentState = getCell y x g
      in case (currentState, aliveNeighbors) of
        (1, 2) -> 1  -- Live cell with 2 neighbors lives on
        (1, 3) -> 1  -- Live cell with 3 neighbors lives on
        (0, 3) -> 1  -- Dead cell with exactly 3 neighbors becomes alive
        _      -> 0  -- All other cells die or stay dead
    
    -- Count alive neighbors for a cell at position (y,x)
    countAliveNeighbors y x g = sum $ map (\(ny, nx) -> getCell ny nx g) neighborPositions
      where
        neighborPositions = [(y+dy, x+dx) | dy <- [-1,0,1], dx <- [-1,0,1], not (dy == 0 && dx == 0)]
    
    -- Get the value of a cell at position (y,x)
    getCell y x g
      | y < 0 || y >= height || x < 0 || x >= width = 0
      | otherwise = (g !! y) !! x
