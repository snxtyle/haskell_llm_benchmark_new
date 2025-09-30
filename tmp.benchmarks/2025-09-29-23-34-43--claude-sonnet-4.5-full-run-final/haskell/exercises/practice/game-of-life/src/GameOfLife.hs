module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid = [[nextCellState row col | col <- [0..width-1]] | row <- [0..height-1]]
  where
    height = length grid
    width = if height > 0 then length (head grid) else 0
    
    nextCellState row col =
      let current = getCell row col
          neighbors = countLiveNeighbors row col
      in if current == 1
         then if neighbors == 2 || neighbors == 3 then 1 else 0
         else if neighbors == 3 then 1 else 0
    
    getCell row col
      | row < 0 || row >= height = 0
      | col < 0 || col >= width = 0
      | otherwise = grid !! row !! col
    
    countLiveNeighbors row col =
      sum [getCell r c | r <- [row-1..row+1], 
                         c <- [col-1..col+1],
                         not (r == row && c == col)]
