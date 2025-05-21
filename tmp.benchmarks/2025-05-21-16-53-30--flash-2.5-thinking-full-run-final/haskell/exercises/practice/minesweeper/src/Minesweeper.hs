module Minesweeper (annotate) where

import Data.Char (intToDigit)

-- | Annotates a Minesweeper board by counting adjacent mines for empty squares.
--   Mines ('*') remain unchanged. Empty squares (' ') with no adjacent mines
--   also remain unchanged. Empty squares with adjacent mines are replaced
--   by the digit representing the count.
annotate :: [String] -> [String]
annotate board =
    let
        -- Determine the number of rows and columns in the board.
        rows = length board
        cols = if null board then 0 else length (head board)

        -- Helper function to check if a given coordinate (r, c) is within the board boundaries.
        isValid :: Int -> Int -> Bool
        isValid r c = r >= 0 && r < rows && c >= 0 && c < cols

        -- Helper function to check if a cell at (r, c) contains a mine.
        -- It first checks if the coordinate is valid.
        isMine :: Int -> Int -> Bool
        isMine r c = isValid r c && (board !! r !! c == '*')

        -- Helper function to count the number of adjacent mines for a cell at (r, c).
        -- It checks all 8 surrounding cells.
        countAdjacentMines :: Int -> Int -> Int
        countAdjacentMines r c =
            let
                -- Relative coordinates for the 8 neighboring cells.
                neighbors =
                    [ (-1, -1), (-1, 0), (-1, 1) -- Top row
                    , ( 0, -1),           ( 0, 1) -- Middle row (excluding self)
                    , ( 1, -1), ( 1, 0), ( 1, 1)  -- Bottom row
                    ]
            in
                -- Sums 1 for each neighbor that is a valid mine.
                sum [1 | (dr, dc) <- neighbors, isMine (r + dr) (c + dc)]

        -- Helper function to process a single cell at (r, c).
        -- It determines the character for the annotated board at this position.
        processCell :: Int -> Int -> Char
        processCell r c =
            case board !! r !! c of
                '*' -> '*' -- If it's a mine, it remains a mine.
                ' ' ->
                    let
                        mineCount = countAdjacentMines r c
                    in
                        if mineCount == 0
                            then ' ' -- If no adjacent mines, it remains an empty space.
                            else intToDigit mineCount -- Otherwise, convert the count to a digit character.
                other -> other -- Handles any other unexpected characters (though problem implies only ' ' or '*').
    in
        -- Use nested list comprehensions to build the new board.
        -- Outer comprehension iterates through rows, inner through columns.
        [ [processCell r c | c <- [0..cols-1]] | r <- [0..rows-1] ]
