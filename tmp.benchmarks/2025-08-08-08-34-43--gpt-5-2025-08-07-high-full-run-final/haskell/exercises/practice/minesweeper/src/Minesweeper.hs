module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate rows = map annotateRow (zip [0 ..] rows)
  where
    nRows :: Int
    nRows = length rows

    annotateRow :: (Int, String) -> String
    annotateRow (r, row) = map (annotateCell r) (zip [0 ..] row)

    annotateCell :: Int -> (Int, Char) -> Char
    annotateCell r (c, ch)
      | ch == '*' = '*'
      | otherwise =
          let n = countNeighbors r c
           in if n == 0 then ' ' else intToDigit n

    countNeighbors :: Int -> Int -> Int
    countNeighbors r c =
      length
        [ ()
        | dr <- [-1, 0, 1]
        , dc <- [-1, 0, 1]
        , not (dr == 0 && dc == 0)
        , let rr = r + dr
        , let cc = c + dc
        , inBounds rr cc
        , isMine rr cc
        ]

    inBounds :: Int -> Int -> Bool
    inBounds rr cc =
      rr >= 0 && rr < nRows
        && cc >= 0 && cc < rowLen rr

    rowLen :: Int -> Int
    rowLen rr = length (rows !! rr)

    isMine :: Int -> Int -> Bool
    isMine rr cc = (rows !! rr) !! cc == '*'
