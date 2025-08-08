module Alphametics (solve) where

import Data.Char (isAlpha, toUpper)
import Data.List (nub, permutations)
import Data.Maybe (listToMaybe)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe solutions
  where
    -- Parse the equation
    (leftSide, rightSide) = parseEquation puzzle
    allWords = leftSide ++ [rightSide]
    
    -- Get unique letters
    letters = nub $ filter isAlpha $ map toUpper puzzle
    
    -- Get letters that cannot be zero (first letters of words)
    nonZeroLetters = nub $ map (toUpper . head) $ filter (not . null) allWords
    
    -- Try all possible digit assignments
    solutions = [ assignment |
                  digits <- chooseDigits (length letters),
                  let assignment = zip letters digits,
                  isValidAssignment assignment nonZeroLetters,
                  satisfiesEquation assignment leftSide rightSide ]
    
    -- Generate all permutations of n digits from 0-9
    chooseDigits n = if n > 10 then [] else permutations [0..9] >>= \perm -> [take n perm]

-- Parse the equation string into left side words and right side word
parseEquation :: String -> ([String], String)
parseEquation s = 
    let parts = words $ map cleanChar s
        (left, right) = splitAtEquals parts
    in (left, head right)
  where
    cleanChar c
        | isAlpha c = toUpper c
        | c == '=' = ' '
        | c == '+' = ' '
        | otherwise = c
    
    splitAtEquals xs = 
        case break (== "==") xs of
            (before, _:after) -> (filter (not . null) before, concat after)
            _ -> case reverse xs of
                    (r:ls) -> (reverse ls, r)
                    _ -> ([], "")

-- Check if assignment is valid (no leading zeros)
isValidAssignment :: [(Char, Int)] -> [Char] -> Bool
isValidAssignment assignment nonZeroLetters =
    all (\c -> lookupDigit c assignment /= Just 0) nonZeroLetters

-- Check if the assignment satisfies the equation
satisfiesEquation :: [(Char, Int)] -> [String] -> String -> Bool
satisfiesEquation assignment leftWords rightWord =
    case (leftValues, rightValue) of
        (Just lvals, Just rval) -> sum lvals == rval
        _ -> False
  where
    leftValues = mapM (wordToNumber assignment) leftWords
    rightValue = wordToNumber assignment rightWord

-- Convert a word to its numeric value given letter assignments
wordToNumber :: [(Char, Int)] -> String -> Maybe Int
wordToNumber assignment word = do
    digits <- mapM (\c -> lookupDigit (toUpper c) assignment) word
    return $ foldl (\acc d -> acc * 10 + d) 0 digits

-- Lookup a digit for a character
lookupDigit :: Char -> [(Char, Int)] -> Maybe Int
lookupDigit c assignment = lookup c assignment
