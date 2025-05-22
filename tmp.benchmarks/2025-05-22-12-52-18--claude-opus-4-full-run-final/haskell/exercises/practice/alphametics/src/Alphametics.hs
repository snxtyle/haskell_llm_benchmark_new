module Alphametics (solve) where

import Data.List (nub, permutations)
import Data.Maybe (listToMaybe)
import Data.Char (isAlpha, toUpper)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe solutions
  where
    -- Parse the puzzle into operands and result
    (operands, result) = parsePuzzle puzzle
    
    -- Get all unique letters
    letters = nub $ concatMap (filter isAlpha . map toUpper) (result : operands)
    
    -- Get letters that cannot be zero (first letter of multi-letter words)
    nonZeroLetters = nub $ concatMap firstLetterIfMulti (result : operands)
    
    -- Try all possible digit assignments
    solutions = [ assignment |
                  digits <- take (length letters) <$> permutations [0..9],
                  let assignment = zip letters digits,
                  isValidAssignment assignment nonZeroLetters,
                  isValidEquation operands result assignment ]

-- Parse puzzle string into operands and result
parsePuzzle :: String -> ([String], String)
parsePuzzle s = (operands, result)
  where
    parts = words $ map clean s
    clean c = if isAlpha c || c == ' ' then c else ' '
    (operands, result) = case break (== "=") parts of
      (ops, "=" : [res]) -> (ops, res)
      _ -> (init parts, last parts)

-- Get first letter of a word if it has multiple letters
firstLetterIfMulti :: String -> [Char]
firstLetterIfMulti word = 
  let cleaned = filter isAlpha $ map toUpper word
  in if length cleaned > 1 then [head cleaned] else []

-- Check if assignment respects leading zero constraints
isValidAssignment :: [(Char, Int)] -> [Char] -> Bool
isValidAssignment assignment nonZeroLetters =
  all (\c -> lookupDigit c assignment /= Just 0) nonZeroLetters

-- Check if the equation is valid with given assignment
isValidEquation :: [String] -> String -> [(Char, Int)] -> Bool
isValidEquation operands result assignment =
  case (mapM (wordToNumber assignment) operands, wordToNumber assignment result) of
    (Just opNums, Just resNum) -> sum opNums == resNum
    _ -> False

-- Convert a word to a number using the letter-digit assignment
wordToNumber :: [(Char, Int)] -> String -> Maybe Int
wordToNumber assignment word = do
  let cleaned = filter isAlpha $ map toUpper word
  digits <- mapM (`lookupDigit` assignment) cleaned
  return $ foldl (\acc d -> acc * 10 + d) 0 digits

-- Lookup a digit for a letter in the assignment
lookupDigit :: Char -> [(Char, Int)] -> Maybe Int
lookupDigit c assignment = lookup (toUpper c) assignment
