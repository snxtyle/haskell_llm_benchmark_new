module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board
  | null board = []
  | otherwise = [ [ getChar r c | c <- [0..cols-1] ] | r <- [0..rows-1] ]
  where
    rows = length board
    cols = length (head board)

    getChar :: Int -> Int -> Char
    getChar r c
      | board !! r !! c == '*' = '*'
      | otherwise =
          let count = countMines r c
          in if count == 0 then ' ' else head (show count)

    countMines :: Int -> Int -> Int
    countMines r c = length $ filter isMine neighborCoords
      where
        neighborCoords = [(r + dr, c + dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], not (dr == 0 && dc == 0)]
        isMine (nr, nc) = nr >= 0 && nr < rows && nc >= 0 && nc < cols && (board !! nr) !! nc == '*'
