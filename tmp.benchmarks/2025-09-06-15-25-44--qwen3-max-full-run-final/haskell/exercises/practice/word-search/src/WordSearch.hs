module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe (listToMaybe)

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

-- Main function to search for words in the grid
search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = [(word, findWord grid word) | word <- wordList]

-- Find a word in the grid, returning its start and end positions if found
findWord :: [String] -> String -> Maybe WordPos
findWord grid word
    | null word = Nothing
    | otherwise = listToMaybe [WordPos startPos endPos | 
                               startPos@(CharPos x y) <- allPositions,
                               dir <- directions,
                               let endPos = findEndInDirection grid word startPos dir,
                               endPos /= startPos]
  where
    height = length grid
    width = if null grid then 0 else length (grid !! 0)  -- Use index 0 instead of head
    allPositions = [CharPos x y | y <- [0..height-1], x <- [0..width-1]]
    directions = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], not (dx == 0 && dy == 0)]

-- Try to find the end position of a word starting from a given position in a direction
findEndInDirection :: [String] -> String -> CharPos -> (Int, Int) -> CharPos
findEndInDirection grid word startPos@(CharPos x y) (dx, dy) =
    if matchesWord grid word startPos (dx, dy)
    then CharPos (x + dx * (length word - 1)) (y + dy * (length word - 1))
    else startPos  -- Return start position if word doesn't match

-- Check if a word matches starting from a position in a given direction
matchesWord :: [String] -> String -> CharPos -> (Int, Int) -> Bool
matchesWord _ [] _ _ = True  -- Use _ instead of grid since it's not used
matchesWord grid (c:cs) (CharPos x y) (dx, dy) =
    if y < 0 || y >= length grid || x < 0 || x >= (if null grid then 0 else length (grid !! y))
    then False
    else if (grid !! y) !! x /= c
         then False
         else matchesWord grid cs (CharPos (x + dx) (y + dy)) (dx, dy)
