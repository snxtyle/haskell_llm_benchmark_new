module Minesweeper (annotate) where

import Data.Char (intToDigit)

-- | Annotate an ASCII representation of a completed Minesweeper board.
--
--   Each cell in the input is either a mine ('*') or a blank space (' ').
--   The returned board keeps the mines unchanged and replaces blank spaces
--   with either another blank (if no adjacent mines) or the count of adjacent
--   mines (as a digit character).
--
--   Adjacent cells are the eight surrounding cells: horizontally, vertically,
--   and diagonally.
--
--   The function is total; it gracefully handles empty boards or boards with
--   zero width.
annotate :: [String] -> [String]
annotate board = [ [ annotateCell r c | c <- [0 .. width r - 1] ]
                 | (r, _) <- indexedRows
                 ]
  where
    indexedRows = zip [0 ..] board

    -- Width of a given row (rows may theoretically differ in length).
    width r = length (board !! r)

    -- Determine the character to place at position (r, c).
    annotateCell :: Int -> Int -> Char
    annotateCell r c =
      case cell of
        '*' -> '*'                  -- Keep mines unchanged.
        ' ' ->
          if mines == 0
            then ' '                -- Leave blank if no adjacent mines.
            else intToDigit mines   -- Otherwise, show the count.
        _   -> cell                 -- For robustness, leave any other char.
      where
        cell  = board !! r !! c
        mines = countAdjacentMines r c

    -- Count mines in the 8 neighbouring cells of (r, c).
    countAdjacentMines :: Int -> Int -> Int
    countAdjacentMines r c =
      length
        [ ()
        | dr <- deltas
        , dc <- deltas
        , (dr, dc) /= (0, 0)
        , let r' = r + dr
        , let c' = c + dc
        , inBounds r' c'
        , board !! r' !! c' == '*'
        ]
      where
        deltas = [-1, 0, 1]

    -- Check if coordinates are inside the board.
    inBounds :: Int -> Int -> Bool
    inBounds r c =
      r >= 0
        && r < length board
        && c >= 0
        && c < width r
