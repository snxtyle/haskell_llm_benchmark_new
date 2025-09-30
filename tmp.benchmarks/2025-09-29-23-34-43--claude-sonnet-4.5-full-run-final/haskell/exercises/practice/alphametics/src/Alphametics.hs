module Alphametics (solve) where

import Data.Char (isAlpha, isSpace)
import Data.List (nub, (\\))
import Data.Maybe (listToMaybe)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = 
    case parseEquation puzzle of
        Nothing -> Nothing
        Just (leftWords, rightWord) ->
            let letters = nub $ filter isAlpha puzzle
                leadingLetters = nub $ map head $ filter (not . null) (leftWords ++ [rightWord])
                solution = findSolution letters leadingLetters leftWords rightWord
            in fmap (zip letters) solution

-- Parse the puzzle into left side words and right side word
parseEquation :: String -> Maybe ([String], String)
parseEquation puzzle =
    let cleaned = filter (\c -> isAlpha c || c == '+' || c == '=' || isSpace c) puzzle
        parts = words cleaned
        (leftPart, rightPart) = break (== "==") $ filter (/= "+") parts
    in case rightPart of
        ("==":result:_) -> Just (leftPart, result)
        _ -> case break (== "=") $ filter (/= "+") parts of
            (left, ("=":result:_)) -> Just (left, result)
            _ -> Nothing

-- Find a valid digit assignment for the letters
findSolution :: String -> String -> [String] -> String -> Maybe [Int]
findSolution letters leadingLetters leftWords rightWord =
    listToMaybe $ filter isValid $ generateAssignments letters leadingLetters []
  where
    isValid assignment =
        let mapping = zip letters assignment
            leftValues = map (wordToNumber mapping) leftWords
            rightValue = wordToNumber mapping rightWord
        in all (/= Nothing) (rightValue : leftValues) &&
           sum (map (\(Just n) -> n) leftValues) == (\(Just n) -> n) rightValue

-- Generate all valid digit assignments using backtracking
generateAssignments :: String -> String -> [Int] -> [[Int]]
generateAssignments [] _ usedDigits = [reverse usedDigits]
generateAssignments (letter:rest) leadingLetters usedDigits =
    let availableDigits = [0..9] \\ usedDigits
        validDigits = if letter `elem` leadingLetters
                      then filter (/= 0) availableDigits
                      else availableDigits
    in concatMap (\digit -> generateAssignments rest leadingLetters (digit:usedDigits)) validDigits

-- Convert a word to its numeric value given a letter-to-digit mapping
wordToNumber :: [(Char, Int)] -> String -> Maybe Int
wordToNumber mapping word =
    let digits = map (\c -> lookup c mapping) word
    in if all (/= Nothing) digits
       then Just $ foldl (\acc (Just d) -> acc * 10 + d) 0 digits
       else Nothing
