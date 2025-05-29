module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick [] = []
tick grid =
    let rows = length grid
        cols = length (head grid)
        get r c
            | r < 0 || r >= rows = 0
            | c < 0 || c >= cols = 0
            | otherwise = (grid !! r) !! c
        neighbors i j = sum [ get (i+dx) (j+dy)
                            | dx <- [-1,0,1]
                            , dy <- [-1,0,1]
                            , (dx,dy) /= (0,0)
                            ]
        setCell i j = let
            current = get i j
            count = neighbors i j
            in
                if count == 3 || (current == 1 && count == 2) 
                then 1 
                else 0
    in [ [ setCell i j | j <- [0..cols-1] ] | i <- [0..rows-1] ]
