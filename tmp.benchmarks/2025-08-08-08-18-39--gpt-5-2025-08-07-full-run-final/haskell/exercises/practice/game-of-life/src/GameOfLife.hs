module GameOfLife (tick) where

-- Compute the next generation of Conway's Game of Life
-- Input: a grid of 0s (dead) and 1s (alive)
-- Output: the next generation grid with the same shape
tick :: [[Int]] -> [[Int]]
tick g =
  [ [ nextCell (cellAt g r c) (neighborsOf g r c)
    | c <- [0 .. length row - 1]
    ]
  | (r, row) <- zip [0 ..] g
  ]

-- Safe indexing into a list
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise               = Just (xs !! i)

-- Get the value of a cell, treating out-of-bounds as dead (0)
-- Any non-zero value is treated as alive (1)
cellAt :: [[Int]] -> Int -> Int -> Int
cellAt g r c =
  case safeIndex g r of
    Nothing  -> 0
    Just row ->
      case safeIndex row c of
        Nothing -> 0
        Just v  -> if v == 0 then 0 else 1

-- Count live neighbors around (r, c)
neighborsOf :: [[Int]] -> Int -> Int -> Int
neighborsOf g r c =
  sum
    [ cellAt g (r + dr) (c + dc)
    | dr <- [-1, 0, 1]
    , dc <- [-1, 0, 1]
    , (dr, dc) /= (0, 0)
    ]

-- Apply Game of Life rules to a single cell
nextCell :: Int -> Int -> Int
nextCell curr n
  | curr == 1 && (n == 2 || n == 3) = 1
  | curr == 0 && n == 3             = 1
  | otherwise                       = 0
