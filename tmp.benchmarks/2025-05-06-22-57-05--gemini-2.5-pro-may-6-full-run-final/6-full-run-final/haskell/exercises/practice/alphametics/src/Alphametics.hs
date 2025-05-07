module Alphametics (solve) where

import Control.Monad (guard) -- Removed redundant mapM import
import Data.Foldable (asum)
import Data.List (delete, findIndex, nub, sort)

-- Helper to parse the puzzle string. Example: "SEND + MORE = MONEY"
-- Returns Nothing for invalid puzzle format.
parseEquation' :: String -> Maybe ([String], String)
parseEquation' puzzle = do -- Using Maybe monad (do-notation)
    let ws = words puzzle
    eqIndex <- findIndex (== "=") ws -- findIndex is in Data.List
    
    -- Ensure resultWord exists by checking eqIndex against length
    guard (eqIndex + 1 < length ws)
    let lhsWords = take eqIndex ws
        resultWord = ws !! (eqIndex + 1)
        -- Filter out "+" and any empty strings that might result from parsing
        addendWords = filter (\w -> w /= "+" && w /= "") lhsWords
    
    -- Basic validation of parsed components
    guard (not (null addendWords)) -- Must have at least one addend
    let allPuzzleWords = addendWords ++ [resultWord]
    guard (all (not . null) allPuzzleWords) -- Ensure no words are empty strings
    guard (all (all (`elem` ['A'..'Z'])) allPuzzleWords) -- Ensure words contain only uppercase letters
    
    return (addendWords, resultWord)

-- Convert a word to its numerical value using the current letter-to-digit mapping.
-- Returns Nothing if a leading zero is assigned to a multi-digit number,
-- or if a character in the word is not in the mapping (should not happen with correct logic).
wordToNumber' :: [(Char, Int)] -> String -> Maybe Integer
wordToNumber' mapping word =
    -- Check for leading zero in a multi-digit number.
    -- Single-letter words can be 0.
    if length word > 1 && lookup (head word) mapping == Just 0
    then Nothing
    else go 0 word
  where
    go :: Integer -> String -> Maybe Integer -- Accumulator for the number being built
    go acc [] = Just acc -- Base case: end of word, return accumulated number
    go acc (c:cs) =
        case lookup c mapping of
            Just digit -> go (acc * 10 + toInteger digit) cs -- Recursively build number
            Nothing    -> Nothing -- Character not found in mapping

-- Main function to solve the alphametics puzzle.
solve :: String -> Maybe [(Char, Int)]
solve puzzle =
    case parseEquation' puzzle of
        Nothing -> Nothing -- Puzzle string is malformed or invalid
        Just (addendWords, resultWord) ->
            let allWords = addendWords ++ [resultWord]
                -- Extract all unique characters from the puzzle.
                uniqueChars = nub $ concat allWords
                
                -- Identify characters that cannot be zero (leading characters of multi-digit words).
                leadingChars = nub [head w | w <- allWords, length w > 1]
            in
            -- Optimization: if more unique characters than digits [0..9], no solution.
            if length uniqueChars > 10
            then Nothing
            else -- Proceed to the backtracking solver.
                 runBacktrackingSolver uniqueChars leadingChars addendWords resultWord

-- Backtracking solver to find a valid assignment of digits to characters.
runBacktrackingSolver :: [Char]      -- List of unique characters to assign digits to.
                      -> [Char]      -- List of characters that cannot be assigned zero.
                      -> [String]    -- List of addend words.
                      -> String      -- The result word.
                      -> Maybe [(Char, Int)] -- The solution mapping, if found.
runBacktrackingSolver uniqueChars_list leadingChars_list addWords resWord =
    solver [] uniqueChars_list [0..9] -- Initial call to recursive helper
  where
    -- Recursive helper function for backtracking.
    solver :: [(Char, Int)] -- Current mapping being built.
           -> [Char]      -- Remaining characters to assign.
           -> [Int]       -- Available digits.
           -> Maybe [(Char, Int)]
    solver currentMapping [] _availableDigits = -- Base case: all characters assigned.
        -- Check if the current mapping satisfies the arithmetic equation.
        if checkEquationSolution currentMapping addWords resWord
        then Just (sort currentMapping) -- Sort for canonical output.
        else Nothing                    -- Mapping is invalid.
    
    solver currentMapping (charToAssign : remainingChars) availableDigits =
        -- Try assigning each available digit to charToAssign.
        -- asum takes a list of Maybe results and returns the first Just, or Nothing if all are Nothing.
        asum (map (tryDigitForChar charToAssign remainingChars availableDigits) availableDigits)
      where
        -- Inner helper to try one specific digit for charToAssign.
        -- It captures 'currentMapping' and 'leadingChars_list' from the outer scope.
        tryDigitForChar :: Char -> [Char] -> [Int] -> Int -> Maybe [(Char, Int)]
        tryDigitForChar char remInnerChars currentAvailableDigits digitToTry =
            -- Pruning: if charToAssign is a leading character and digitToTry is 0, this path is invalid.
            if char `elem` leadingChars_list && digitToTry == 0
            then Nothing
            else
                -- Recursively call solver with the new mapping and updated available digits.
                let newMapping = (char, digitToTry) : currentMapping
                    -- `delete` removes the first occurrence of digitToTry from currentAvailableDigits.
                    nextAvailableDigits = delete digitToTry currentAvailableDigits 
                in solver newMapping remInnerChars nextAvailableDigits

-- Checks if a complete mapping satisfies the sum equation.
checkEquationSolution :: [(Char, Int)] -> [String] -> String -> Bool
checkEquationSolution mappingList addWords resWord =
    -- Convert all addend words to numbers. mapM handles Maybe results.
    case mapM (wordToNumber' mappingList) addWords of
        Nothing -> False -- One of the addends was invalid (e.g. leading zero).
        Just addendValues ->
            -- Convert the result word to a number.
            case wordToNumber' mappingList resWord of
                Nothing -> False -- Result word was invalid.
                Just resultValue -> sum addendValues == resultValue -- Check sum.
