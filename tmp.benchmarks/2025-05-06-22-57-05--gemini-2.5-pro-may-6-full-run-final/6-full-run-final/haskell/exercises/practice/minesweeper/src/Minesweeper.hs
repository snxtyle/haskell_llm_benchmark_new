module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board
  -- Handle empty board or board with empty rows appropriately.
  -- The problem implies a non-empty rectangular board.
  | null board || null (head board) = board
  | otherwise =
      [ [ processCell r c cellChar
        | (c, cellChar) <- zip [0..] rowString ]
      | (r, rowString) <- zip [0..] board ]
  where
    numRows :: Int
    numRows = length board

    numCols :: Int
    numCols = length (head board)

    -- Check if a given coordinate (row, col) is within the board's bounds.
    isValid :: Int -> Int -> Bool
    isValid r c = r >= 0 && r < numRows && c >= 0 && c < numCols

    -- Count mines adjacent to the cell (r, c).
    countAdjacentMines :: Int -> Int -> Int
    countAdjacentMines r c =
      length $ filter (== '*') $ map getCellAtNighbor validNeighbors
      where
        -- Generate all 8 potential neighbor relative coordinates
        deltas = [(dr, dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0,0)]
        
        -- Calculate absolute coordinates of potential neighbors
        potentialNeighborsCoords = map (\(dr, dc) -> (r + dr, c + dc)) deltas
        
        -- Filter to keep only neighbors that are within board boundaries
        validNeighbors = filter (uncurry isValid) potentialNeighborsCoords

        -- Get character at a specific (valid) board coordinate
        getCellAtNighbor :: (Int, Int) -> Char
        getCellAtNighbor (nr, nc) = board !! nr !! nc

    -- Determine the character for a cell (r, c) with its original content.
    processCell :: Int -> Int -> Char -> Char
    processCell r c currentCellChar
      | currentCellChar == '*' = '*' -- Mines remain mines
      | otherwise = -- Empty square, count adjacent mines
          let mineCount = countAdjacentMines r c
          in if mineCount == 0
               then ' ' -- No adjacent mines, remains empty
               else intToDigit mineCount -- Otherwise, show the count
