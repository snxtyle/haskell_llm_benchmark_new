module Alphametics (solve) where

import Data.Char (isAlpha, isDigit, toUpper)
import Data.List (nub, find, delete, partition)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)

type Assignment = M.Map Char Int
type Domain = M.Map Char [Int]

-- Main function to solve alphametics puzzles
solve :: String -> Maybe [(Char, Int)]
solve puzzle = case solutions of
  [] -> Nothing
  (sol:_) -> Just $ M.toList sol
  where
    -- Parse the puzzle
    (addends, sumWord) = parsePuzzle puzzle
    allLetters = nub $ concat (addends ++ [sumWord])
    leadingLetters = map head (addends ++ [sumWord])
    
    -- Create initial domain (0-9 for each letter, but 1-9 for leading letters)
    initialDomain = M.fromList [(c, if c `elem` leadingLetters then [1..9] else [0..9]) | c <- allLetters]
    
    -- Generate all possible solutions
    solutions = solveWithConstraints addends sumWord allLetters initialDomain M.empty

-- Parse the puzzle string into addends and sum
parsePuzzle :: String -> ([String], String)
parsePuzzle puzzle = (lhsWords, rhsWord)
  where
    -- Split by '=' to get left and right sides
    sides = splitOn "=" puzzle
    lhs = head sides
    rhs = last sides
    
    -- Split left side by '+' to get addends
    lhsWords = filter (not . null) $ map (filter (/= ' ') . map toUpper) $ splitOn "+" lhs
    rhsWord = filter (/= ' ') $ map toUpper rhs

-- Solve using backtracking with constraints
solveWithConstraints :: [String] -> String -> [Char] -> Domain -> Assignment -> [Assignment]
solveWithConstraints addends sumWord [] _ assignment
  | isValidSolution addends sumWord assignment = [assignment]
  | otherwise = []
solveWithConstraints addends sumWord (var:vars) domain assignment = 
  concat [solveWithConstraints addends sumWord vars newDomain (M.insert var value assignment)
         | value <- M.findWithDefault [] var domain
         , isConsistent var value assignment]
  where
    newDomain = M.map (delete value) domain
    isConsistent _ _ _ = True  -- Could add more constraints here for efficiency

-- Check if the assignment makes a valid solution
isValidSolution :: [String] -> String -> Assignment -> Bool
isValidSolution addends sumWord assignment =
  sum addendValues == sumValue
  where
    addendValues = map (wordToNumber assignment) addends
    sumValue = wordToNumber assignment sumWord

-- Convert a word to a number using the assignment
wordToNumber :: Assignment -> String -> Int
wordToNumber assignment word = 
  foldl (\acc c -> acc * 10 + M.findWithDefault 0 c assignment) 0 word
