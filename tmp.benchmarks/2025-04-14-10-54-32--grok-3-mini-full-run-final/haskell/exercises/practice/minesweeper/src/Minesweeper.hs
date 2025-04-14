module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board
  | null board = []
  | otherwise = [ [ annotateCell board i j | j <- [0..n-1] ] | i <- [0..m-1] ]
  where
    m = length board  -- Number of rows
    n = if m > 0 then length (head board) else 0  -- Number of columns

annotateCell :: [String] -> Int -> Int -> Char
annotateCell board i j
  | board!!i!!j == '*' = '*'  -- Mine remains a mine
  | otherwise = let count = sum [ if inBounds di dj && isMine di dj then 1 else 0 | di <- [-1..1], dj <- [-1..1], (di, dj) /= (0, 0) ]
                in if count > 0 then head (show count) else ' '  -- Convert count to char if > 0
  where
    inBounds di dj = let newI = i + di; newJ = j + dj
                     in newI >= 0 && newI < m && newJ >= 0 && newJ < n
    isMine di dj = board!!(i + di)!!(j + dj) == '*'
    m = length board
    n = if m > 0 then length (head board) else 0
