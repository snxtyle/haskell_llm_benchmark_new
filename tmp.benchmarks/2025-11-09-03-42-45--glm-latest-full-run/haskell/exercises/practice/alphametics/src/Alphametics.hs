module Alphametics (solve) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub, foldl', elemIndex, (\\))
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Monad (guard)

-- | Solves an alphametics puzzle.
-- Takes a puzzle string like "SEND + MORE = MONEY" and returns a Just
-- a list of (Char, Int) pairs representing the solution, or Nothing if no solution exists.
solve :: String -> Maybe [(Char, Int)]
solve puzzle = do
    let (lhsWords, rhsWord) = parsePuzzle puzzle
    let allLetters = nub $ concat (lhsWords ++ [rhsWord])
    let leadingLetters = nub $ map head (lhsWords ++ [rhsWord])
    finalMapping <- findSolution allLetters leadingLetters lhsWords rhsWord
    return $ Map.toList finalMapping

-- | Parses a puzzle string into a list of LHS words and a single RHS word.
-- Example: "A + B = C" -> (["A", "B"], "C")
parsePuzzle :: String -> ([String], String)
parsePuzzle puzzle = (lhsWords, rhsWord)
  where
    parts = words puzzle
    eqIndex = case elemIndex "=" parts of
                Just i -> i
                Nothing -> error "Malformed puzzle: no '='"
    lhsRaw = take eqIndex parts
    rhsWord = case drop (eqIndex + 1) parts of
                (w:_) -> w
                []    -> error "Malformed puzzle: no RHS"
    lhsWords = filter (/= "+") lhsRaw

-- | Finds a valid letter-to-digit mapping using a backtracking search.
findSolution :: [Char] -> [Char] -> [String] -> String -> Maybe (Map Char Int)
findSolution lettersToAssign leadingLetters lhsWords rhsWord = go lettersToAssign Map.empty
  where
    -- Base case: all letters have been assigned. Check if the mapping is a valid solution.
    go [] mapping =
        if checkSolution mapping
        then Just mapping
        else Nothing

    -- Recursive step: assign a digit to the next letter and recurse.
    go (l:rest) mapping =
        let usedDigits = Map.elems mapping
            possibleDigits = [0..9] \\ usedDigits
            -- Apply the leading-zero constraint.
            allowedDigits = if l `elem` leadingLetters
                            then filter (/= 0) possibleDigits
                            else possibleDigits
        -- Try each allowed digit. The first one that leads to a solution is returned.
        in listToMaybe $ mapMaybe (\d -> go rest (Map.insert l d mapping)) allowedDigits

    -- Checks if a complete mapping satisfies the puzzle equation.
    checkSolution :: Map Char Int -> Bool
    checkSolution mapping =
        let lhsSum = sum $ map (`wordToNum` mapping) lhsWords
            rhsNum = wordToNum rhsWord mapping
        in lhsSum == rhsNum

-- | Converts a word to its numerical value using a given letter-to-digit mapping.
wordToNum :: String -> Map Char Int -> Int
wordToNum word mapping = foldl' (\acc c -> acc * 10 + (mapping Map.! c)) 0 word
