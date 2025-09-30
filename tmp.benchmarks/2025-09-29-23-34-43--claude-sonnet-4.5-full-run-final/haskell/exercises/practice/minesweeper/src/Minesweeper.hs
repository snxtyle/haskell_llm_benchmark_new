module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = 
  [ [ annotateCell row col | (col, _) <- zip [0..] rowStr ]
  | (row, rowStr) <- zip [0..] board
  ]
  where
    rows = length board
    cols = if rows == 0 then 0 else length (head board)
    
    annotateCell :: Int -> Int -> Char
    annotateCell row col =
      let cell = (board !! row) !! col
      in if cell == '*'
         then '*'
         else let count = countAdjacentMines row col
              in if count == 0
                 then ' '
                 else intToDigit count
    
    countAdjacentMines :: Int -> Int -> Int
    countAdjacentMines row col =
      length [ ()
             | r <- [row - 1 .. row + 1]
             , c <- [col - 1 .. col + 1]
             , r >= 0 && r < rows
             , c >= 0 && c < cols
             , not (r == row && c == col)
             , (board !! r) !! c == '*'
             ]
