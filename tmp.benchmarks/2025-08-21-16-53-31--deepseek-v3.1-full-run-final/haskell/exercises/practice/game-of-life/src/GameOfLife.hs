module GameOfLife (tick) where

import Data.List (transpose)

-- Count live neighbors for a cell at position (x, y)
countNeighbors :: [[Int]] -> Int -> Int -> Int
countNeighbors grid x y = sum [grid !! ny !! nx | 
    ny <- [max 0 (y-1) .. min (length grid - 1) (y+1)],
    nx <- [max 0 (x-1) .. min (length (head grid) - 1) (x+1)],
    not (ny == y && nx == x)]  -- Exclude the cell itself

-- Apply the game rules to a single cell
applyRules :: [[Int]] -> Int -> Int -> Int
applyRules grid x y = 
    let neighbors = countNeighbors grid x y
        current = grid !! y !! x
    in case current of
        1 -> if neighbors `elem` [2, 3] then 1 else 0
        0 -> if neighbors == 3 then 1 else 0
        _ -> 0

-- Generate the next generation
tick :: [[Int]] -> [[Int]]
tick grid = 
    [[applyRules grid x y | x <- [0..width-1]] | y <- [0..height-1]]
    where
        height = length grid
        width = if height > 0 then length (head grid) else 0
