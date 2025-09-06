module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid = [[nextCellState (grid !! y !! x) (countLiveNeighbors grid x y) | x <- [0..width-1]] | y <- [0..height-1]]
  where
    height = length grid
    width = if height > 0 then length (head grid) else 0
    countLiveNeighbors g x y = sum [getCellValue g (x + dx) (y + dy) | dx <- [-1..1], dy <- [-1..1], not (dx == 0 && dy == 0)]
    getCellValue g cx cy
      | cx < 0 || cx >= width || cy < 0 || cy >= height = 0
      | otherwise = g !! cy !! cx
    nextCellState current liveNeighbors
      | current == 1 && liveNeighbors `elem` [2, 3] = 1  -- Live cell with 2 or 3 neighbors survives
      | current == 0 && liveNeighbors == 3 = 1          -- Dead cell with exactly 3 neighbors becomes alive
      | otherwise = 0                                   -- All other cells die or stay dead
