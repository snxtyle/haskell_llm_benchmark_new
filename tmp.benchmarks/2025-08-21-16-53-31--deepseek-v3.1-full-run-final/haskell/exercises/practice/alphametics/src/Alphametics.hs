module Alphametics (solve) where

import Data.Char (isUpper)
import Data.List (nub, (\\))
import Data.Maybe (listToMaybe)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe solutions
  where
    -- Parse the puzzle
    cleanPuzzle = filter (/= ' ') puzzle
    (left, right) = break (== '=') cleanPuzzle
    rightSide = drop 1 right  -- Skip the '='
    leftWords = words $ map (\c -> if c == '+' then ' ' else c) left
    rightWord = rightSide
    
    -- Get all unique letters and non-zero letters
    puzzleLetters = nub $ filter isUpper cleanPuzzle
    nonZeroLetters = nub $ map head (leftWords ++ [rightWord])
    
    -- Find solutions
    solutions = [assignment | assignment <- generateAssignments puzzleLetters nonZeroLetters,
                             checkEquation leftWords rightWord assignment]

-- Generate all possible assignments of digits to letters
generateAssignments :: [Char] -> [Char] -> [[(Char, Int)]]
generateAssignments letters nonZeroLetters = 
    [zip letters digits | digits <- permutations [0..9] (length letters),
     all (\(c, d) -> c `notElem` nonZeroLetters || d /= 0) (zip letters digits)]

-- Check if the equation holds with the given assignment
checkEquation :: [String] -> String -> [(Char, Int)] -> Bool
checkEquation leftWords rightWord assignment = 
    let wordValue word = foldl (\acc c -> acc * 10 + charValue c) 0 word
        charValue c = case lookup c assignment of
                       Just v -> v
                       Nothing -> error "Missing character in assignment"
        leftSum = sum $ map wordValue leftWords
        rightSum = wordValue rightWord
    in leftSum == rightSum

-- Helper function to generate permutations of n elements from a list
permutations :: Eq a => [a] -> Int -> [[a]]
permutations _ 0 = [[]]
permutations xs n = 
    [y:zs | y <- xs, zs <- permutations (xs \\ [y]) (n-1)]
