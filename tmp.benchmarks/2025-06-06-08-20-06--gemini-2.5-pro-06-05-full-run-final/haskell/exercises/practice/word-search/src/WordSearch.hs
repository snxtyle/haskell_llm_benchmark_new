module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe (catMaybes, listToMaybe)

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search [] wordList = map (\w -> (w, Nothing)) wordList
search grid@(firstRow:_) wordList
  | null firstRow = map (\w -> (w, Nothing)) wordList
  | otherwise = map (\word -> (word, findWordFor word)) wordList
  where
    -- Grid dimensions
    height = length grid
    width = length firstRow

    -- All possible starting positions in the grid (0-indexed)
    allPositions = [CharPos{col=c, row=r} | r <- [0..height-1], c <- [0..width-1]]

    -- All 8 directions (horizontal, vertical, diagonal)
    directions = [(dr, dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0,0)]

    -- Tries to find a single word in the grid.
    findWordFor :: String -> Maybe WordPos
    findWordFor word
      | null word = Nothing
      | otherwise = listToMaybe $ concatMap (searchFrom word) allPositions

    -- For a given starting position, searches for the word in all directions.
    searchFrom :: String -> CharPos -> [WordPos]
    searchFrom word startPos = catMaybes [checkDirection word startPos dir | dir <- directions]

    -- Checks if a word can be found starting at `startPos` in direction `dir`.
    checkDirection :: String -> CharPos -> (Int, Int) -> Maybe WordPos
    checkDirection word startPos@(CharPos{col=c, row=r}) (dr, dc) =
      let
        wordLen = length word
        -- Generate 0-indexed coordinates for each character of the word
        coords = [(r + i * dr, c + i * dc) | i <- [0..wordLen-1]]
      in
        if all inBounds coords then
          let
            extractedWord = map (\(rowIdx, colIdx) -> (grid !! rowIdx) !! colIdx) coords
          in
            if extractedWord == word then
              let
                (endRow, endCol) = last coords
                -- Convert 0-indexed internal coordinates to 1-indexed for output
                startPos1Based = CharPos{col = c + 1, row = r + 1}
                endPos1Based = CharPos{col = endCol + 1, row = endRow + 1}
              in Just WordPos{start=startPos1Based, end=endPos1Based}
            else
              Nothing
        else
          Nothing

    -- Checks if a coordinate (row, col) is within the grid boundaries.
    inBounds :: (Int, Int) -> Bool
    inBounds (r, c) = r >= 0 && r < height && c >= 0 && c < width
