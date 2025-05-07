module WordProblem (answer) where

import Data.List (isPrefixOf, isSuffixOf)
import Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer problem =
    -- 1. Validate overall structure "What is ... ?"
    -- Ensure the problem starts with "What is " and ends with "?"
    if not ("What is " `isPrefixOf` problem && "?" `isSuffixOf` problem)
        then Nothing
        else
            -- 2. Extract the core part of the problem
            let prefixLength = length "What is "
                suffixLength = 1 -- length of "?"
                -- Calculate the length of the actual problem statement
                coreLength = length problem - prefixLength - suffixLength
            in -- If coreLength is negative, it means the problem string was too short (e.g., "What is?")
               if coreLength < 0
               then Nothing
               else
                 -- Extract the core string (e.g., "5 plus 10")
                 let coreProblemString = take coreLength $ drop prefixLength problem
                 -- If the extracted core string is empty (e.g., problem was "What is ?"), it's invalid.
                 in if null coreProblemString
                    then Nothing
                    else parseAndEvaluate (words coreProblemString)

-- Parses the first number from the list of words and then
-- hands off to `evaluateOperations` to process subsequent operations.
parseAndEvaluate :: [String] -> Maybe Integer
parseAndEvaluate [] = Nothing -- This case handles if `words coreProblemString` results in an empty list (e.g. core was only spaces)
parseAndEvaluate (firstToken:restTokens) =
    -- The first token must be a number.
    case readMaybe firstToken :: Maybe Integer of
        Nothing -> Nothing -- Not a number, so invalid problem.
        Just initialValue -> evaluateOperations initialValue restTokens -- Start evaluating operations with this initial value.

-- Recursively evaluates operations one by one, from left to right.
-- Takes the current accumulated value and the remaining tokens.
evaluateOperations :: Integer -> [String] -> Maybe Integer
evaluateOperations acc [] = Just acc -- Base case: No more tokens, all operations processed. Return accumulator.

-- Pattern for two-word operations like "multiplied by" or "divided by".
-- Expects: operation_word "by" number_word ...
evaluateOperations acc (opWord : "by" : numStr : restTokens)
    | opWord == "multiplied" =
        case readMaybe numStr of
            Just val -> evaluateOperations (acc * val) restTokens
            Nothing  -> Nothing -- Operand is not a valid number.
    | opWord == "divided" =
        case readMaybe numStr of
            Just val -> if val == 0
                        then Nothing -- Division by zero is an error.
                        else evaluateOperations (acc `div` val) restTokens
            Nothing  -> Nothing -- Operand is not a valid number.
    -- If opWord was something else but was followed by "by" (e.g., "foo by 7"),
    -- it doesn't match "multiplied" or "divided", so it falls through to the catch-all error case.

-- Pattern for single-word operations like "plus" or "minus".
-- Expects: operation_word number_word ...
evaluateOperations acc (opWord : numStr : restTokens)
    | opWord == "plus" =
        case readMaybe numStr of
            Just val -> evaluateOperations (acc + val) restTokens
            Nothing  -> Nothing -- Operand is not a valid number.
    | opWord == "minus" =
        case readMaybe numStr of
            Just val -> evaluateOperations (acc - val) restTokens
            Nothing  -> Nothing -- Operand is not a valid number.
    -- If opWord was not "plus" or "minus" (e.g., "cubed", or a number like "5" where an op was expected),
    -- it falls through to the catch-all error case.

-- Catch-all for any other token sequence, indicating an error:
-- - Unknown operation (e.g., "What is 5 cubed 2?")
-- - Insufficient tokens for an operation (e.g., "What is 5 plus?", "What is 5 multiplied by?")
-- - Operator expected but number found (e.g., "What is 1 2 plus 3?")
-- - Malformed multi-word operator (e.g. "What is 5 multiplied 7?")
evaluateOperations _ _ = Nothing
