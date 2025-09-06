module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid = [[ newCell i j | j <- [0..cols-1]] | i <- [0..rows-1]]
  where
    rows = length grid
    cols = if null grid then 0 else maximum (0 : map length grid)
    newCell i j = let current = if j < length (grid !! i) then grid !! i !! j else 0
                      count = sum [ if ni >= 0 && ni < rows && nj >= 0 && nj < cols
                                    then if nj < length (grid !! ni) then grid !! ni !! nj else 0
                                    else 0
                                   | di <- [-1..1]
                                   , dj <- [-1..1]
                                   , let ni = i + di
                                   , let nj = j + dj
                                   , not (di == 0 && dj == 0) ]
                  in if current == 1 then if count `elem` [2,3] then 1 else 0 else if count == 3 then 1 else 0
