module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate [] = []
annotate board = 
    [ [ if cell == ' ' 
        then countMines board row col 
        else cell 
      | (col, cell) <- zip [0..] rowContent 
      ] 
    | (row, rowContent) <- zip [0..] board 
    ]

countMines :: [String] -> Int -> Int -> Char
countMines board row col = 
    let mineCount = length $ filter (== '*') $ neighbors board row col
    in if mineCount == 0 then ' ' else head (show mineCount)

neighbors :: [String] -> Int -> Int -> String
neighbors board row col = 
    [ getCell board r c 
    | dr <- [-1, 0, 1]
    , dc <- [-1, 0, 1]
    , let r = row + dr
    , let c = col + dc
    , (dr, dc) /= (0, 0)
    , inBounds board r c
    ]

getCell :: [String] -> Int -> Int -> Char
getCell board row col = (board !! row) !! col

inBounds :: [String] -> Int -> Int -> Bool
inBounds board row col = 
    row >= 0 && row < length board && 
    col >= 0 && col < length (head board)
