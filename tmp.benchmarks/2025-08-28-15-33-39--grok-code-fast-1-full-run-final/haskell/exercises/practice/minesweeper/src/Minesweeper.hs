module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = [[annotateCell i j | j <- [0..width-1]] | i <- [0..height-1]]
  where
    height = length board
    width = if height > 0 then length (head board) else 0
    annotateCell i j =
      let cell = board !! i !! j
      in if cell == '*'
         then '*'
         else let count = sum [1 | di <- [-1..1], dj <- [-1..1],
                                   not (di == 0 && dj == 0),
                                   let ni = i + di
                                       nj = j + dj
                                   in ni >= 0 && ni < height &&
                                      nj >= 0 && nj < width &&
                                      board !! ni !! nj == '*']
              in if count == 0 then ' ' else intToDigit count
