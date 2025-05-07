module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board =
  -- Check for empty board
  if null board || null (head board)
    then board
    else
      let height = length board
          width = length (head board)

          -- Safely get the character at (r, c) from the original board
          getChar :: Int -> Int -> Maybe Char
          getChar r c
            | r < 0 || r >= height || c < 0 || c >= width = Nothing -- Out of bounds
            | otherwise = Just $ (board !! r) !! c

          -- Count adjacent mines for a given cell (r, c)
          countAdjacentMines :: Int -> Int -> Int
          countAdjacentMines r c =
            length $ filter (== Just '*') $ -- Count how many neighbors are mines
              [ getChar nr nc -- Get character of neighbor
              | dr <- [-1, 0, 1], dc <- [-1, 0, 1], -- Iterate through relative offsets (-1, 0, 1)
                (dr, dc) /= (0, 0), -- Exclude the cell itself
                let nr = r + dr, -- Calculate neighbor row
                let nc = c + dc -- Calculate neighbor column
              ]

          -- Determine the character for the annotated cell at (r, c)
          annotateCell :: Int -> Int -> Char -> Char
          annotateCell r c cell
            | cell == '*' = '*' -- Mines remain mines
            | otherwise =
                let count = countAdjacentMines r c
                 in if count == 0
                      then ' ' -- No adjacent mines, remain empty
                      else intToDigit count -- Convert count (1-8) to character '1'-'8'

      -- Iterate through each row with its index r
       in zipWith
            ( \r rowStr ->
                -- Iterate through each character in the row with its index c
                zipWith (\c char -> annotateCell r c char) [0 ..] rowStr
            )
            [0 ..]
            board
