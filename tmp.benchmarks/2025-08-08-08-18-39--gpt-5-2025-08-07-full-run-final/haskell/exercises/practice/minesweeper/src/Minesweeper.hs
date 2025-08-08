module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board =
  [ [ annotateCell r c | (c, _) <- zip [0..] row ]
  | (r, row) <- zip [0..] board
  ]
  where
    annotateCell :: Int -> Int -> Char
    annotateCell r c =
      case charAt r c of
        '*' -> '*'
        ' ' ->
          let n = countMines r c
          in if n == 0 then ' ' else intToDigit n
        ch -> ch

    charAt :: Int -> Int -> Char
    charAt r c = (board !! r) !! c

    inBounds :: Int -> Int -> Bool
    inBounds r c =
      r >= 0 && r < length board &&
      c >= 0 && c < length (board !! r)

    countMines :: Int -> Int -> Int
    countMines r c =
      length
        [ ()
        | dr <- [-1..1]
        , dc <- [-1..1]
        , dr /= 0 || dc /= 0
        , let r' = r + dr
        , let c' = c + dc
        , inBounds r' c'
        , charAt r' c' == '*'
        ]
