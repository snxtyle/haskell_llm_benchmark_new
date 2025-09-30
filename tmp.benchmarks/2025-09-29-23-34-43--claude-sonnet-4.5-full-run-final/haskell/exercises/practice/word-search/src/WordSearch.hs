module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.List (find)
import Data.Char (toLower)

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord grid word)) wordList

findWord :: [String] -> String -> Maybe WordPos
findWord grid word 
    | null grid = Nothing
    | otherwise = 
        let positions = [(r, c) | r <- [0..length grid - 1], c <- [0..length (grid !! 0) - 1]]
            directions = [(0,1), (0,-1), (1,0), (-1,0), (1,1), (1,-1), (-1,1), (-1,-1)]
            searches = [searchFrom grid word r c dr dc | (r, c) <- positions, (dr, dc) <- directions]
        in find (\x -> x /= Nothing) searches >>= id

searchFrom :: [String] -> String -> Int -> Int -> Int -> Int -> Maybe WordPos
searchFrom grid word startRow startCol deltaRow deltaCol =
    let wordLower = map toLower word
        gridHeight = length grid
        gridWidth = if null grid then 0 else length (grid !! 0)
        
        getCharAt r c 
            | r < 0 || r >= gridHeight || c < 0 || c >= gridWidth = Nothing
            | otherwise = Just (toLower ((grid !! r) !! c))
        
        positions = take (length word) [(startRow + i * deltaRow, startCol + i * deltaCol) | i <- [0..]]
        
        chars = sequence [getCharAt r c | (r, c) <- positions]
        
    in case chars of
        Just foundChars | foundChars == wordLower ->
            let endRow = startRow + (length word - 1) * deltaRow
                endCol = startCol + (length word - 1) * deltaCol
            in Just (WordPos (CharPos (startCol + 1) (startRow + 1)) (CharPos (endCol + 1) (endRow + 1)))
        _ -> Nothing
