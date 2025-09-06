module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = map (map annotateCell) indexedBoard
  where
    rows = length board
    cols = if null board then 0 else length (head board)
    
    indexedBoard = [[(i, j) | j <- [0..cols-1]] | i <- [0..rows-1]]
    
    annotateCell (i, j)
      | board !! i !! j == '*' = '*'
      | count == 0             = ' '
      | otherwise              = intToDigit count
      where
        count = countAdjacentMines i j
    
    countAdjacentMines i j = length $ filter isMine neighbors
      where
        neighbors = [(x, y) | x <- [i-1..i+1], y <- [j-1..j+1],
                             x >= 0, x < rows, y >= 0, y < cols,
                             (x, y) /= (i, j)]
        
        isMine (x, y) = board !! x !! y == '*'
