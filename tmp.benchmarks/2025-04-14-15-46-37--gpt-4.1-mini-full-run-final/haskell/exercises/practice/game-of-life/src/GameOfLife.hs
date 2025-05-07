module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick board = [[nextState r c | c <- [0..cols-1]] | r <- [0..rows-1]]
  where
    rows = length board
    cols = if rows == 0 then 0 else length (head board)

    getCell r c
      | r < 0 || r >= rows = 0
      | c < 0 || c >= cols = 0
      | otherwise = (board !! r) !! c

    neighbors r c =
      sum [ getCell (r + dr) (c + dc)
          | dr <- [-1,0,1]
          , dc <- [-1,0,1]
          , (dr, dc) /= (0,0)
          ]

    nextState r c =
      let alive = getCell r c
          liveNeighbors = neighbors r c
      in case alive of
           1 -> if liveNeighbors == 2 || liveNeighbors == 3 then 1 else 0
           0 -> if liveNeighbors == 3 then 1 else 0
           _ -> 0
