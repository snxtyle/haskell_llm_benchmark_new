module Alphametics (solve) where

import Data.Char (isAlpha, isUpper)
import Data.List (nub, permutations, find)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map

-- Parse the puzzle into (summands, result)
parsePuzzle :: String -> ([String], String)
parsePuzzle s =
    let (lhs, rhs) = break (== '=') s
        lhsWords = map (filter isAlpha) $ words $ map (\c -> if c == '+' then ' ' else c) lhs
        rhsWord = filter isAlpha $ dropWhile (not . isAlpha) rhs
    in (lhsWords, rhsWord)

-- Get all unique letters in the puzzle, in order of appearance
uniqueLetters :: [String] -> String -> [Char]
uniqueLetters summands result = nub $ concat (summands ++ [result])

-- Get the set of leading letters (cannot be zero)
leadingLetters :: [String] -> String -> [Char]
leadingLetters summands result = nub $ map head (filter (not . null) (summands ++ [result]))

-- Convert a word to a number using a mapping
wordToNumber :: Map.Map Char Int -> String -> Int
wordToNumber m = foldl (\acc c -> acc * 10 + m Map.! c) 0

-- Try all possible assignments
solve :: String -> Maybe [(Char, Int)]
solve puzzle =
    let (summands, result) = parsePuzzle puzzle
        letters = uniqueLetters summands result
        n = length letters
        leads = leadingLetters summands result
        digits = [0..9]
        -- Generate all possible assignments of digits to letters
        possibleAssignments = filter (\xs -> all (\(c, d) -> if c `elem` leads then d /= 0 else True) (zip letters xs))
                                $ permutations digits
        tryAssignment xs =
            let m = Map.fromList (zip letters xs)
                sumWords = map (wordToNumber m) summands
                resultWord = wordToNumber m result
            in if sum sumWords == resultWord
                then Just (zip letters xs)
                else Nothing
    in find (/= Nothing) (map tryAssignment possibleAssignments) >>= id
