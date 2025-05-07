module Alphametics (solve) where

import Data.Char (isUpper)
import Data.List (nub, permutations, find)
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import Control.Monad (guard)

-- Function to parse the puzzle string into addends and the result word.
-- Example: "SEND + MORE == MONEY" -> Just (["SEND", "MORE"], "MONEY")
parsePuzzle :: String -> Maybe ([String], String)
parsePuzzle puzzle =
    let parts = words $ filter (\c -> isUpper c || c `elem` "+=") puzzle
        isValidPart p = not (null p) && all isUpper p
    in case break (== "==") parts of
        (addendParts, "==" : resultParts) ->
            let addends = filter (/= "+") addendParts
                result = concat resultParts -- Should be a single word
            in if not (null addends) && all isValidPart addends && isValidPart result
               then Just (addends, result)
               else Nothing
        _ -> Nothing

-- Function to convert a word to an integer given a letter-to-digit mapping.
-- Returns Nothing if a letter is not in the map or if a multi-digit number starts with 0.
wordToNum :: Map.Map Char Int -> String -> Maybe Integer
wordToNum mapping word = do
    digits <- mapM (`Map.lookup` mapping) word
    case digits of
        (0:_) | length word > 1 -> Nothing -- Leading zero in multi-digit number
        ds                      -> Just $ foldl (\acc d -> acc * 10 + fromIntegral d) 0 ds

-- Main function to solve the alphametics puzzle.
solve :: String -> Maybe [(Char, Int)]
solve puzzle =
    case parsePuzzle puzzle of
        Nothing -> Nothing -- Invalid puzzle format
        Just (addends, result) ->
            let allWords = addends ++ [result]
                uniqueLetters = nub $ filter isUpper $ concat allWords
                -- Leading letters of multi-digit words cannot be zero.
                leadingLetters = nub [head w | w <- allWords, length w > 1]
                numUniqueLetters = length uniqueLetters
                digits = [0..9]

                -- Generate all possible assignments of unique digits to the unique letters.
                possibleSolutions = do
                    -- Select 'numUniqueLetters' digits from [0..9] and permute them.
                    digitPermutation <- permutations digits
                    let assignmentList = zip uniqueLetters digitPermutation
                    let assignmentMap = Map.fromList assignmentList

                    -- Constraint: Leading letters of multi-digit words cannot be 0.
                    guard $ all (\c -> Map.lookup c assignmentMap /= Just 0) leadingLetters

                    -- Convert words to numbers using the current assignment.
                    maybeAddendValues <- mapM (wordToNum assignmentMap) addends
                    maybeResultValue  <- wordToNum assignmentMap result

                    -- Constraint: The arithmetic sum must hold true.
                    guard $ sum maybeAddendValues == maybeResultValue

                    -- If all constraints are met, this is a valid solution.
                    return assignmentList

            -- Return the first valid solution found, if any.
            in listToMaybe possibleSolutions
