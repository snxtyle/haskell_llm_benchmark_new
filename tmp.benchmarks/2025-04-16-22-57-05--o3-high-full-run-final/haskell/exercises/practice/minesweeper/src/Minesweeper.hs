module Minesweeper (annotate) where

import Data.Char (intToDigit)

-- | Given a Minesweeper board, add mine counts to each empty square.
--   Mines are represented by '*', empty squares by ' ' (space).
--   For every empty square, replace it with the number of adjacent
--   mines.  If no adjacent mines are present, leave the square empty.
annotate :: [String] -> [String]
annotate board = [ annotateRow r row | (r, row) <- zip [0 ..] board ]
  where
    -- All relative positions we must inspect around a given square
    deltas :: [Int]
    deltas = [-1, 0, 1]

    -- Safely retrieve the character at position (r, c) if it exists
    safeGet :: Int -> Int -> Maybe Char
    safeGet r c
        | r < 0 || r >= length board      = Nothing
        | c < 0 || c >= length (board !! r) = Nothing
        | otherwise                       = Just ((board !! r) !! c)

    -- Determine if the square at (r, c) contains a mine
    isMine :: Int -> Int -> Bool
    isMine r c = case safeGet r c of
        Just '*' -> True
        _        -> False

    -- Produce an annotated version of one row
    annotateRow :: Int -> String -> String
    annotateRow r row = [ annotateCell r c | c <- [0 .. length row - 1] ]

    -- Produce the correct character for a single cell
    annotateCell :: Int -> Int -> Char
    annotateCell r c =
        case safeGet r c of
            Just '*' -> '*'                       -- Keep mines unchanged
            Just _   ->
                let count = length
                          [ () | dr <- deltas
                               , dc <- deltas
                               , (dr, dc) /= (0, 0)
                               , isMine (r + dr) (c + dc)
                          ]
                in if count == 0
                      then ' '                     -- Leave empty if zero
                      else intToDigit count        -- Otherwise show count
            Nothing -> error "Invalid board: index out of bounds."
