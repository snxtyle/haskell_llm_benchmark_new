module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid =
  [ [ newCell i j | j <- [0 .. cols - 1] ] | i <- [0 .. rows - 1] ]
  where
    rows = length grid
    cols = if rows > 0 then length (head grid) else 0
    
    newCell i j
      | i < 0 || i >= rows || j < 0 || j >= cols = 0  -- Out of bounds cells are dead
      | otherwise =
          let neighbors = countNeighbors i j
              current = grid !! i !! j
          in if current == 1
             then if neighbors == 2 || neighbors == 3 then 1 else 0
             else if neighbors == 3 then 1 else 0
    
    countNeighbors i j =
      sum
        [ if inBounds x y && (x, y) /= (i, j) then grid !! x !! y else 0
          | x <- [i - 1 .. i + 1]
          , y <- [j - 1 .. j + 1]
        ]
    
    inBounds x y = x >= 0 && x < rows && y >= 0 && y < cols
