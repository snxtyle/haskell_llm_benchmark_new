module WordProblem (answer) where

import Data.List (isPrefixOf, isSuffixOf)
import Text.Read (readMaybe)
import Control.Monad (guard)

-- | Evaluates a simple math word problem.
-- Returns 'Just result' on success, or 'Nothing' if the problem is invalid.
answer :: String -> Maybe Integer
answer problem
    -- Check for the required prefix and suffix
    | not ("What is " `isPrefixOf` problem) = Nothing
    | not ("?" `isSuffixOf` problem) = Nothing
    | otherwise = 
        let -- Extract the core expression from the problem string
            core = drop (length "What is ") (take (length problem - 1) problem)
            -- Tokenize the core expression into a list of words
            tokens = words core
        in eval tokens

-- | Recursively evaluates a list of tokens.
-- The list is expected to be in the format [num, op, num, op, num, ...]
eval :: [String] -> Maybe Integer
eval (numStr:rest) = do
    -- The first token must be a number. readMaybe handles parsing errors.
    num <- readMaybe numStr
    -- Start the recursive evaluation with the initial number.
    eval' num rest
  where
    -- Helper function to perform the left-to-right evaluation.
    eval' :: Integer -> [String] -> Maybe Integer
    eval' acc [] = Just acc -- Base case: no more tokens, return the final result.
    
    -- Pattern match for addition
    eval' acc ("plus":numStr:xs) = do
        n <- readMaybe numStr
        eval' (acc + n) xs

    -- Pattern match for subtraction
    eval' acc ("minus":numStr:xs) = do
        n <- readMaybe numStr
        eval' (acc - n) xs

    -- Pattern match for multiplication (handles two-word operation)
    eval' acc ("multiplied":"by":numStr:xs) = do
        n <- readMaybe numStr
        eval' (acc * n) xs

    -- Pattern match for division (handles two-word operation)
    eval' acc ("divided":"by":numStr:xs) = do
        n <- readMaybe numStr
        guard (n /= 0) -- Prevent division by zero
        eval' (acc `div` n) xs

    -- Any other pattern is a syntax error or unsupported operation.
    eval' _ _ = Nothing

-- An empty token list is not a valid problem.
eval [] = Nothing
