module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = 
    [ [ annotateCell row col | col <- [0..width-1] ]
    | row <- [0..height-1] 
    ]
  where
    height = length board
    width = if height > 0 then length (head board) else 0
    
    annotateCell :: Int -> Int -> Char
    annotateCell row col
        | isMine row col = '*'
        | mineCount > 0  = intToDigit mineCount
        | otherwise      = ' '
      where
        mineCount = countAdjacentMines row col
    
    isMine :: Int -> Int -> Bool
    isMine row col = 
        row >= 0 && row < height && 
        col >= 0 && col < width && 
        board !! row !! col == '*'
    
    countAdjacentMines :: Int -> Int -> Int
    countAdjacentMines row col = length
        [ () 
        | dr <- [-1, 0, 1]
        , dc <- [-1, 0, 1]
        , not (dr == 0 && dc == 0)  -- Don't count the cell itself
        , isMine (row + dr) (col + dc)
        ]
