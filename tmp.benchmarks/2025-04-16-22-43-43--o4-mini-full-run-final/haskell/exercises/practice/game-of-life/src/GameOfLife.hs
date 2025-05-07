module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid = 
  let rows = length grid
      cols = if rows == 0 then 0 else length (head grid)
      getCell i j
        | i >= 0, i < rows, j >= 0, j < cols = (grid !! i) !! j
        | otherwise                          = 0
      liveNeighbors i j =
        sum [ getCell (i + di) (j + dj)
            | di <- [-1,0,1], dj <- [-1,0,1]
            , not (di == 0 && dj == 0)
            ]
      nextState i j =
        let ln   = liveNeighbors i j
            cell = getCell i j
        in case (cell, ln) of
             (1, 2) -> 1
             (_, 3) -> 1
             _      -> 0
  in [ [ nextState i j | j <- [0..cols-1] ]
     | i <- [0..rows-1]
     ]
