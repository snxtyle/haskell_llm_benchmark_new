module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid = [[newCell row col | col <- [0..width-1]] | row <- [0..height-1]]
  where
    height = length grid
    width = if height == 0 then 0 else length (head grid)
    
    newCell row col = 
      let currentCell = getCell row col
          neighbors = countLiveNeighbors row col
      in case (currentCell, neighbors) of
           (1, 2) -> 1  -- live cell with 2 neighbors survives
           (1, 3) -> 1  -- live cell with 3 neighbors survives
           (0, 3) -> 1  -- dead cell with 3 neighbors becomes alive
           _      -> 0  -- all other cases: cell dies or stays dead
    
    getCell row col
      | row < 0 || row >= height || col < 0 || col >= width = 0
      | otherwise = (grid !! row) !! col
    
    countLiveNeighbors row col = sum [getCell (row + dr) (col + dc) | 
                                      dr <- [-1, 0, 1], 
                                      dc <- [-1, 0, 1], 
                                      not (dr == 0 && dc == 0)]
