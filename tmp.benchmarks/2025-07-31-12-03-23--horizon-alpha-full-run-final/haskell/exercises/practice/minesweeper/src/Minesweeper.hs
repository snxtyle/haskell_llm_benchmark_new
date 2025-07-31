module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = [ [ annotateCell r c | c <- [0 .. cols - 1] ] | r <- [0 .. rows - 1] ]
  where
    rows = length board
    cols = if null board then 0 else length (head board)

    inBounds r c = r >= 0 && r < rows && c >= 0 && c < cols

    cellAt r c
      | inBounds r c = (board !! r) !! c
      | otherwise = ' '

    neighbors (r, c) =
      [ (r + dr, c + dc)
      | dr <- [-1, 0, 1]
      , dc <- [-1, 0, 1]
      , not (dr == 0 && dc == 0)
      , inBounds (r + dr) (c + dc)
      ]

    countMines r c = length [ () | (nr, nc) <- neighbors (r, c), cellAt nr nc == '*' ]

    annotateCell r c =
      case cellAt r c of
        '*' -> '*'
        ' ' ->
          let n = countMines r c
          in if n == 0 then ' ' else intToDigit n
        other -> other
