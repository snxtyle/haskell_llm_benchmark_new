module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = 
  [ [ annotateCell r c | c <- [0 .. width - 1] ]
  | r <- [0 .. height - 1]
  ]
  where
    height = length board
    width  = if null board then 0 else length (head board)

    annotateCell :: Int -> Int -> Char
    annotateCell r c =
      case board !! r !! c of
        '*' -> '*'
        _   ->
          let count = length
                [ ()
                | dr <- [-1 .. 1]
                , dc <- [-1 .. 1]
                , (dr, dc) /= (0, 0)
                , let nr = r + dr
                , let nc = c + dc
                , nr >= 0, nr < height
                , nc >= 0, nc < width
                , board !! nr !! nc == '*'
                ]
          in if count == 0 then ' ' else intToDigit count
