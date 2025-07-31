module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid = [[newState row col | col <- [0..(length (head grid) - 1)]] | row <- [0..(length grid - 1)]]
  where
    newState row col = 
      let cell = getCell row col grid
          liveNeighbors = countLiveNeighbors row col grid
      in if cell == 1 
         then if liveNeighbors == 2 || liveNeighbors == 3 then 1 else 0
         else if liveNeighbors == 3 then 1 else 0

countLiveNeighbors :: Int -> Int -> [[Int]] -> Int
countLiveNeighbors row col grid = 
  sum [getCell (row + dr) (col + dc) grid | 
       dr <- [-1, 0, 1], 
       dc <- [-1, 0, 1], 
       (dr, dc) /= (0, 0)]

getCell :: Int -> Int -> [[Int]] -> Int
getCell row col grid
  | null grid || row < 0 || col < 0 || row >= length grid || col >= length (head grid) = 0
  | otherwise = (grid !! row) !! col
