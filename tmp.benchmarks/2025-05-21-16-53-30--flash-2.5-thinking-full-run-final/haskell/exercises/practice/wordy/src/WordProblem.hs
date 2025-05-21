module WordProblem (answer) where

import Data.List (stripPrefix)
import Text.Read (readMaybe) -- For safe parsing of numbers

-- Define data types for operations and tokens
data Operation = Plus | Minus | MultipliedBy | DividedBy deriving (Show, Eq)
data Token = Num Integer | Op Operation deriving (Show, Eq)

-- Function to tokenize the input string into a list of Tokens.
-- It handles multi-word operations ("multiplied by", "divided by")
-- and returns Nothing if an unknown word or an incomplete operation is found.
tokenize :: String -> Maybe [Token]
tokenize s = go (words s) []
  where
    go [] acc = Just (reverse acc) -- Successfully tokenized all words
    go ("plus":rest) acc = go rest (Op Plus : acc)
    go ("minus":rest) acc = go rest (Op Minus : acc)
    go ("multiplied":"by":rest) acc = go rest (Op MultipliedBy : acc)
    go ("divided":"by":rest) acc = go rest (Op DividedBy : acc)
    go (w:rest) acc =
      case readMaybe w of
        Just n -> go rest (Num n : acc) -- Successfully parsed a number
        Nothing -> Nothing -- Encountered an unparseable word or unexpected sequence

-- Function to evaluate a list of tokens from left to right.
-- It expects a sequence like Num Op Num Op Num ...
-- Returns Nothing for invalid sequences (e.g., two numbers in a row, two ops in a row)
-- or for division by zero.
evaluate :: [Token] -> Maybe Integer
evaluate [] = Nothing -- An empty list of tokens after initial parsing is an error
evaluate [Num n] = Just n -- Base case: a single number is the result
evaluate (Num n1 : Op op : Num n2 : rest) =
    let
        -- Perform the operation
        opResult = case op of
            Plus -> Just (n1 + n2)
            Minus -> Just (n1 - n2)
            MultipliedBy -> Just (n1 * n2)
            DividedBy -> if n2 == 0 then Nothing else Just (n1 `div` n2)
    in
        case opResult of
            Just r -> evaluate (Num r : rest) -- Continue evaluation with the result
            Nothing -> Nothing -- Division by zero or other operation error
evaluate _ = Nothing -- Catches invalid token sequences like (Op _ : _), (Num _ : Num _ : _), etc.

-- Helper function to strip a suffix from a string.
-- Similar to Data.List.stripSuffix, but implemented manually for broader compatibility.
stripSuffix :: String -> String -> Maybe String
stripSuffix suffix s =
    let lenS = length s
        lenSuffix = length suffix
    in if lenS >= lenSuffix && drop (lenS - lenSuffix) s == suffix
       then Just (take (lenS - lenSuffix) s)
       else Nothing

answer :: String -> Maybe Integer
answer problem = do
    -- 1. Validate and remove the prefix "What is "
    trimmedPrefix <- stripPrefix "What is " problem

    -- 2. Validate and remove the suffix "?"
    trimmedSuffix <- stripSuffix "?" trimmedPrefix

    -- 3. Tokenize the core expression string
    tokens <- tokenize trimmedSuffix

    -- 4. Validate the initial structure of tokens and evaluate the expression.
    --    - An empty list of tokens (e.g., "What is ?") is invalid.
    --    - An expression starting with an operator (e.g., "What is plus 5?") is invalid.
    case tokens of
        [] -> Nothing
        (Op _ : _) -> Nothing
        _ -> evaluate tokens -- Proceed to evaluate the valid sequence of tokens
