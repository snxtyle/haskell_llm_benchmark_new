module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe (listToMaybe)
import Data.Char (toLower)

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

type Grid = [String]
type Direction = (Int, Int)

-- All 8 possible directions: right, left, down, up, and 4 diagonals
directions :: [Direction]
directions = [ (1,0), (-1,0), (0,1), (0,-1)
             , (1,1), (1,-1), (-1,1), (-1,-1)
             ]

-- Get the character at (col, row) if in bounds
charAt :: Grid -> Int -> Int -> Maybe Char
charAt grid c r =
    if r >= 0 && r < length grid && c >= 0 && c < length (grid !! r)
        then Just (grid !! r !! c)
        else Nothing

-- Try to match a word starting at (col, row) in a given direction
matchWord :: Grid -> String -> (Int, Int) -> Direction -> Maybe WordPos
matchWord grid word (c, r) (dc, dr) =
    let positions = take (length word) $ iterate (\(x, y) -> (x+dc, y+dr)) (c, r)
        chars = map (\(x, y) -> charAt grid x y) positions
    in if all (==True) (zipWith (\mc w -> fmap toLower mc == Just (toLower w)) chars word)
        then let (ec, er) = last positions
             in Just $ WordPos (CharPos c r) (CharPos ec er)
        else Nothing

-- Find the first occurrence of a word in the grid, in reading order (top-to-bottom, left-to-right)
findWord :: Grid -> String -> Maybe WordPos
findWord grid word =
    let rows = length grid
        cols = if null grid then 0 else length (head grid)
        starts = [ (c, r) | r <- [0..rows-1], c <- [0..cols-1] ]
        -- For each start, try all directions, but return the first match in reading order
        findFirst [] = Nothing
        findFirst ((c, r):rest) =
            case listToMaybe [ pos | dir <- directions, Just pos <- [matchWord grid word (c, r) dir] ] of
                Just pos -> Just pos
                Nothing  -> findFirst rest
    in findFirst starts

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = [ (w, findWord grid w) | w <- wordList ]
