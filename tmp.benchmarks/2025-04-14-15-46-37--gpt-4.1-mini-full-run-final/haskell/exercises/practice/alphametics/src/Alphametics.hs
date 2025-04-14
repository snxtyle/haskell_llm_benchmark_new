module Alphametics (solve) where

import Data.Char (isAlpha)
import Data.List (nub, permutations)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map

-- | Solve an alphametics puzzle.
-- The input is a string like "SEND + MORE = MONEY"
-- Returns a list of (Char, Int) pairs mapping letters to digits if a solution exists.
solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe $ do
    let (lhsWords, rhsWord) = parsePuzzle puzzle
        letters = nub $ concat (rhsWord : lhsWords)
        leadingLetters = nub $ map head (rhsWord : lhsWords)
    guard (length letters <= 10)
    digitsPerm <- permutations [0..9]
    let mapping = Map.fromList $ zip letters digitsPerm
    -- Leading letters cannot be zero
    guard $ all (\c -> Map.findWithDefault 0 c mapping /= 0) leadingLetters
    let lhsValues = map (wordValue mapping) lhsWords
        rhsValue' = wordValue mapping rhsWord
    guard $ sum lhsValues == rhsValue'
    return $ Map.toList mapping

-- Parse the puzzle string into (lhsWords, rhsWord)
-- Example: "SEND + MORE = MONEY" -> (["SEND","MORE"], "MONEY")
parsePuzzle :: String -> ([String], String)
parsePuzzle s =
    let (lhs, rhs) = break (== '=') s
        lhsWords = words $ map (\c -> if c == '+' then ' ' else c) lhs
        rhsWord = drop 1 rhs -- drop '='
        rhsWordTrimmed = filter isAlpha rhsWord
    in (lhsWords, rhsWordTrimmed)

-- Convert a word to its numeric value given a mapping from Char to Int
wordValue :: Map.Map Char Int -> String -> Int
wordValue mapping = foldl (\acc c -> acc * 10 + Map.findWithDefault 0 c mapping) 0

-- guard function for list monad
guard :: Bool -> [()]
guard True = [()]
guard False = []
