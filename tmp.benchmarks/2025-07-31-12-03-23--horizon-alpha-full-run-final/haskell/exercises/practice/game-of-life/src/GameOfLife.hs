module GameOfLife (tick) where

-- Compute the next generation of Conway's Game of Life for a grid of 0s and 1s.
-- The grid is treated as a finite board; cells outside the bounds are considered dead.
tick :: [[Int]] -> [[Int]]
tick grid = [ [ nextCell r c | c <- [0 .. cols - 1] ] | r <- [0 .. rows - 1] ]
  where
    rows = length grid
    cols = if null grid then 0 else length (head grid)

    nextCell :: Int -> Int -> Int
    nextCell r c =
      let alive = cell r c == 1
          n = liveNeighbors r c
       in case (alive, n) of
            (True, 2) -> 1
            (True, 3) -> 1
            (False, 3) -> 1
            _ -> 0

    -- Safely read a cell; out-of-bounds cells are treated as 0 (dead).
    cell :: Int -> Int -> Int
    cell r c
      | r < 0 || r >= rows = 0
      | c < 0 || c >= cols = 0
      | otherwise =
          case safeIndex r grid of
            Nothing -> 0
            Just row ->
              case safeIndex c row of
                Nothing -> 0
                Just v -> if v == 0 then 0 else 1

    liveNeighbors :: Int -> Int -> Int
    liveNeighbors r c = sum
      [ cell (r + dr) (c + dc)
      | dr <- [-1, 0, 1]
      , dc <- [-1, 0, 1]
      , not (dr == 0 && dc == 0)
      ]

    -- Safe indexing for lists.
    safeIndex :: Int -> [a] -> Maybe a
    safeIndex i xs
      | i < 0 = Nothing
      | otherwise = case drop i xs of
          (y:_) -> Just y
          []    -> Nothing
