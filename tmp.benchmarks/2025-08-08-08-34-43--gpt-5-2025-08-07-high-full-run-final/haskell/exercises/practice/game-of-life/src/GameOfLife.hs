module GameOfLife (tick) where

-- | Compute the next generation of Conway's Game of Life.
--   Input is a matrix of 0s and 1s (dead/alive).
--   Output has the same shape as input.
tick :: [[Int]] -> [[Int]]
tick grid =
  [ [ next r c | c <- [0 .. width r - 1] ]
  | r <- [0 .. height - 1]
  ]
  where
    height :: Int
    height = length grid

    widths :: [Int]
    widths = map length grid

    width :: Int -> Int
    width r
      | r < 0 || r >= height = 0
      | otherwise            = widths !! r

    aliveAt :: Int -> Int -> Int
    aliveAt r c = clamp01 (safeGet grid r c)

    neighborCount :: Int -> Int -> Int
    neighborCount r c =
      sum
        [ aliveAt (r + dr) (c + dc)
        | dr <- [-1, 0, 1]
        , dc <- [-1, 0, 1]
        , not (dr == 0 && dc == 0)
        ]

    next :: Int -> Int -> Int
    next r c =
      let alive = aliveAt r c == 1
          n     = neighborCount r c
       in case (alive, n) of
            (True, 2)  -> 1
            (True, 3)  -> 1
            (False, 3) -> 1
            _          -> 0

-- Safely get a cell value; out-of-bounds counts as 0 (dead).
safeGet :: [[Int]] -> Int -> Int -> Int
safeGet g r c
  | r < 0 || r >= length g   = 0
  | c < 0 || c >= length row = 0
  | otherwise                = row !! c
  where
    row :: [Int]
    row = g !! r

-- Clamp any non-zero value to 1.
clamp01 :: Int -> Int
clamp01 x = if x == 0 then 0 else 1
