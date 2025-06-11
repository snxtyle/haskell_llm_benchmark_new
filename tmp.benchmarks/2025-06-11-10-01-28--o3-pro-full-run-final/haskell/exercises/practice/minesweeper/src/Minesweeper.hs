module Minesweeper (annotate) where

import Data.Char (intToDigit)

-- | Annotate a Minesweeper board with mine counts.
--
-- The board is represented as a list of equal-length strings.
-- A '*' represents a mine and a space (' ') represents an empty square.
-- For every empty square we count the number of adjacent mines
-- (horizontally, vertically and diagonally).  If the count is zero
-- we leave the square blank; otherwise we replace it with the digit.
--
-- Examples:
--
-- >>> annotate ["*"]
-- ["*"]
--
-- >>> annotate ["   ", " * ", "   "]
-- ["111", "1*1", "111"]
annotate :: [String] -> [String]
annotate board = [ buildRow r row | (r, row) <- zip [0..] board ]
  where
    rows = length board

    -- Build a single annotated row.
    buildRow :: Int -> String -> String
    buildRow r row = [ annotateCell r c ch | (c, ch) <- zip [0..] row ]

    -- Annotate a single cell.
    annotateCell :: Int -> Int -> Char -> Char
    annotateCell r c ch
      | ch == '*' = '*'    -- keep mines unchanged
      | count == 0 = ' '   -- no adjacent mines -> keep blank
      | otherwise = intToDigit count
      where
        count = adjacentMines r c

    -- Count the number of adjacent mines for coordinates (r, c).
    adjacentMines :: Int -> Int -> Int
    adjacentMines r c =
      length
        [ ()
        | dr <- [-1 .. 1]
        , dc <- [-1 .. 1]
        , not (dr == 0 && dc == 0)
        , let r' = r + dr
        , let c' = c + dc
        , validCoord r' c'
        , isMine r' c'
        ]

    -- Check if coordinates are inside the board.
    validCoord :: Int -> Int -> Bool
    validCoord r c =
      r >= 0
        && r < rows
        && c >= 0
        && c < length (board !! r)

    -- Predicate: is there a mine at (r, c)?
    isMine :: Int -> Int -> Bool
    isMine r c = (board !! r) !! c == '*'
