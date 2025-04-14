module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.List (all, zipWith)

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

-- Define the eight possible directions as pairs of (row delta, column delta)
directions :: [(Int, Int)]
directions = [(dr, dc) | dr <- [-1..1], dc <- [-1..1], not (dr == 0 && dc == 0)]

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid words = map (searchWord grid) words

-- Helper function to search for a single word in the grid
searchWord :: [String] -> String -> (String, Maybe WordPos)
searchWord grid word =
    case findWord grid word of
        Just (startPos, endPos) -> (word, Just (WordPos startPos endPos))
        Nothing -> (word, Nothing)

-- Internal function to find the first occurrence of a word
findWord :: [String] -> String -> Maybe (CharPos, CharPos)
findWord grid word
    | null grid || null (head grid) = Nothing  -- Empty grid
    | otherwise =
        let rows = length grid
            cols = length (head grid)
        in listToMaybe [ (CharPos startCol startRow, CharPos endCol endRow)
                       | startRow <- [0..rows-1]
                       , startCol <- [0..cols-1]
                       , (dr, dc) <- directions
                       , let positions = take (length word) $ iterate (\(r, c) -> (r + dr, c + dc)) (startRow, startCol)
                       , length positions == length word  -- Ensure we have enough positions
                       , all inBounds positions  -- All positions must be within bounds
                       , all (\(i, (r, c)) -> grid !! r !! c == word !! i) (zip [0..] positions)
                       , let (endRow, endCol) = last positions  -- End position
                       ]
  where
    inBounds (r, c) = r >= 0 && r < length grid && c >= 0 && c < length (head grid)
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x  -- Return the first match
