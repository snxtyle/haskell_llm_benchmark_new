module Alphametics (solve) where

import Data.Char (isAlpha, isSpace)
import Data.List (nub, foldl')
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map

-- Solve an alphametics puzzle
solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe $ solvePuzzle parsedPuzzle
  where
    parsedPuzzle = parsePuzzle puzzle

-- Data type to represent the puzzle structure
data Puzzle = Puzzle
  { addends :: [String]
  , result :: String
  , letters :: [Char]
  } deriving (Show)

-- Parse the puzzle string into a structured format
parsePuzzle :: String -> Puzzle
parsePuzzle puzzle = Puzzle addends result letters
  where
    -- Split into left and right side of the equation
    sides = splitOn "==" puzzle
    leftSide = head sides
    rightSide = last sides
    -- Split left side into addends
    addends = map trim $ splitOn "+" leftSide
    result = trim rightSide
    -- Get unique letters from the puzzle
    letters = nub $ filter isAlpha $ concat addends ++ result

-- Split a string on a delimiter
splitOn :: String -> String -> [String]
splitOn delim str = go str []
  where
    go [] acc = reverse acc
    go s acc = let (part, rest) = break (== head delim) s
               in go (drop (length delim) rest) (part : acc)

-- Trim whitespace from a string
trim :: String -> String
trim = filter (not . isSpace)

-- Solve the parsed puzzle by trying different digit assignments
solvePuzzle :: Puzzle -> [[(Char, Int)]]
solvePuzzle puzzle = backtrack (letters puzzle) Map.empty (addends puzzle) (result puzzle)

-- Backtracking search to assign digits to letters
backtrack :: [Char] -> Map.Map Char Int -> [String] -> String -> [[(Char, Int)]]
backtrack [] assignment addends result =
  if isValidSolution assignment addends result
    then [Map.toList assignment]
    else []
backtrack (c:cs) assignment addends result =
  concat [backtrack cs (Map.insert c d assignment) addends result
         | d <- [0..9]
         , not (d `elem` Map.elems assignment)]

-- Check if the current assignment forms a valid solution
isValidSolution :: Map.Map Char Int -> [String] -> String -> Bool
isValidSolution assignment addends result =
  let numbers = map (toNumber assignment) addends
      resultNum = toNumber assignment result
  in all (/= Nothing) (resultNum : numbers)
     && sum (map (maybe 0 id) numbers) == maybe 0 id resultNum

-- Convert a word to a number based on the current assignment
toNumber :: Map.Map Char Int -> String -> Maybe Int
toNumber assignment word =
  if length word > 1 && head word `Map.lookup` assignment == Just 0
    then Nothing -- Leading zero in multi-digit number
    else Just $ foldl' (\acc c -> acc * 10 + maybe 0 id (Map.lookup c assignment)) 0 word
