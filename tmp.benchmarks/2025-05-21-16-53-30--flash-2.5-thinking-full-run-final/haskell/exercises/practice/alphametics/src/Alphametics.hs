module Alphametics (solve) where

import Data.List (nub, delete)
import Control.Monad (msum) -- Added for msum

-- | Solves an alphametics puzzle.
--   The input string should be in the format "WORD1 + WORD2 + ... = RESULT".
--   Returns `Just` a list of (Char, Int) pairs representing the solution,
--   or `Nothing` if no solution is found.
solve :: String -> Maybe [(Char, Int)]
solve puzzle =
    let
        -- Parse the puzzle string into operand words and the result word.
        (operandStrs, resultStr) = parsePuzzle puzzle
        -- Collect all unique letters involved in the puzzle.
        allLetters = nub (concat operandStrs ++ resultStr)
        -- Identify letters that are at the beginning of any word (cannot be 0).
        leadingLetters = nub (map head (operandStrs ++ [resultStr]))

        -- Recursive helper function to find a solution.
        -- `lettersToAssign`: The list of letters yet to be assigned a digit.
        -- `availableDigits`: The list of digits (0-9) that have not yet been used.
        -- `currentAssignment`: The partial assignment of letters to digits built so far.
        go :: [Char] -> [Int] -> [(Char, Int)] -> Maybe [(Char, Int)]
        -- Base case: All letters have been assigned. Now, validate the sum.
        go [] _ currentAssignment =
            let
                -- Convert operand words to their numerical values.
                operandNums = map (wordToNum currentAssignment) operandStrs
                -- Convert the result word to its numerical value.
                resultNum = wordToNum currentAssignment resultStr
            in
                -- Check if the sum of operands equals the result.
                if sum operandNums == resultNum
                then Just currentAssignment -- Solution found!
                else Nothing                -- Sum does not match, this assignment is invalid.

        -- Recursive step: Assign a digit to the current letter.
        go (l:ls) availableDigits currentAssignment =
            -- Try assigning each available digit to the current letter 'l'.
            -- `msum` will return the first `Just` result found, effectively
            -- implementing a depth-first search for a solution.
            msum $ map (\d ->
                -- Pruning: Check leading zero constraint immediately.
                -- If the current letter is a leading letter and the digit is 0,
                -- this branch is invalid, so return Nothing.
                if (l `elem` leadingLetters && d == 0)
                then Nothing
                -- Otherwise, recursively try to assign digits to the remaining letters.
                -- Pass the remaining letters, the digit 'd' removed from available digits,
                -- and the updated current assignment.
                else go ls (delete d availableDigits) ((l,d):currentAssignment)
            ) availableDigits

    -- Start the recursive search with all unique letters, all digits (0-9), and an empty assignment.
    in go allLetters [0..9] []

-- | Parses the alphametics puzzle string into a list of operand words and the result word.
--   Example: "SEND + MORE = MONEY" -> (["SEND", "MORE"], "MONEY")
parsePuzzle :: String -> ([String], String)
parsePuzzle s =
    let
        parts = words s -- Split the string into words by whitespace.
        -- Find the part containing "=". `break` splits the list into two parts:
        -- (elements before the predicate is true, elements from where predicate is true onwards)
        (leftParts, rightParts) = break (== "=") parts
        -- The result word is the last element of the `rightParts` (e.g., ["=", "MONEY"] -> "MONEY").
        resultWord = last rightParts
        -- Operand words are the elements in `leftParts` that are not "+".
        operandWords = filter (\x -> x /= "+" && x /= "=") leftParts
    in
        (operandWords, resultWord)

-- | Converts a word string into its numerical value based on a given letter-to-digit assignment.
--   Example: `wordToNum [('S',9), ('E',5)] "SE"` -> 95
wordToNum :: [(Char, Int)] -> String -> Int
wordToNum assignment word =
    -- Fold over the characters of the word, building the number.
    -- `acc` is the accumulated number, `c` is the current character.
    foldl (\acc c ->
        -- Look up the digit value for the character 'c' in the assignment.
        -- If a character is not found, it indicates a logic error in `allLetters` or `go`.
        let digitValue = case lookup c assignment of
                             Just d -> d
                             Nothing -> error $ "Letter '" ++ [c] ++ "' not found in assignment: " ++ show assignment
        in
            acc * 10 + digitValue -- Append the digit to the number.
    ) 0 word -- Start with an accumulated value of 0.
