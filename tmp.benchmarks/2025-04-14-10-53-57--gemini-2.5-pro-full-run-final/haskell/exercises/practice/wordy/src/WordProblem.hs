module WordProblem (answer) where

import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

-- | Parses and evaluates simple math word problems based on the input string.
--
-- Examples:
--   answer "What is 5?" == Just 5
--   answer "What is 5 plus 13?" == Just 18
--   answer "What is 7 minus 5?" == Just 2
--   answer "What is 6 multiplied by 4?" == Just 24
--   answer "What is 25 divided by 5?" == Just 5
--   answer "What is 3 plus 2 multiplied by 3?" == Just 15
--   answer "What is 5 plus 13 plus 6?" == Just 24
--   answer "What is 52 cubed?" == Nothing
--   answer "Who is the President of the United States?" == Nothing
--   answer "What is 1 plus plus 2?" == Nothing
--   answer "What is?" == Nothing
--
-- Returns the integer result wrapped in Maybe, or Nothing if the input is invalid
-- or describes an unsupported operation.
answer :: String -> Maybe Integer
answer problem = do
    -- 1. Check prefix "What is " and suffix "?" and extract the expression.
    expr <- stripQuestionMaybe problem
    -- 2. Tokenize the expression by spaces.
    tokens <- case words expr of
                [] -> Nothing -- Empty expression is invalid (e.g., "What is?")
                ws -> Just ws
    -- 3. The first token must be a number.
    initialNumStr <- listToMaybe tokens -- Safely get the first token
    initialNum <- readMaybe initialNumStr -- Try reading it as an Integer
    -- 4. Parse the remaining tokens recursively, starting with the initial number.
    parseExpr initialNum (tail tokens)

-- | Strips "What is " prefix and "?" suffix from the input string.
-- Returns Nothing if the string doesn't match the expected format "What is ...?".
-- Also returns Nothing if the content between "What is " and "?" is empty.
stripQuestionMaybe :: String -> Maybe String
stripQuestionMaybe s
    | take 8 s == "What is " && not (null s) && last s == '?' =
        let core = init $ drop 8 s
        in if null core then Nothing else Just core -- Handle "What is?" case
    | otherwise = Nothing

-- | Recursively parses the expression tokens.
-- Takes the current accumulated value and the list of remaining tokens.
-- Returns 'Just result' if parsing succeeds, 'Nothing' otherwise.
parseExpr :: Integer -> [String] -> Maybe Integer
parseExpr acc [] = Just acc -- Base case: Successfully parsed the whole expression

-- Handle operations followed by a number
parseExpr acc ("plus":numStr:rest) = parseNumAndApply acc (+) numStr rest
parseExpr acc ("minus":numStr:rest) = parseNumAndApply acc (-) numStr rest
parseExpr acc ("multiplied":"by":numStr:rest) = parseNumAndApply acc (*) numStr rest
parseExpr acc ("divided":"by":numStr:rest) = parseNumAndApply acc div numStr rest -- Using integer division

-- Error Cases for invalid structure or unknown operations:
parseExpr _ ("plus":[]) = Nothing -- Operator without operand at the end
parseExpr _ ("minus":[]) = Nothing -- Operator without operand at the end
parseExpr _ ("multiplied":[]) = Nothing -- Incomplete "multiplied by" at the end
parseExpr _ ("divided":[]) = Nothing -- Incomplete "divided by" at the end
parseExpr _ ("multiplied":"by":[]) = Nothing -- Operator phrase without operand at the end
parseExpr _ ("divided":"by":[]) = Nothing -- Operator phrase without operand at the end

-- Catch-all for any other invalid sequence or unknown words/operators.
-- This includes cases like:
-- - "What is 1 2?" (number followed by number) -> parseExpr 1 ["2"] -> Nothing
-- - "What is 5 cubed?" (unknown operation) -> parseExpr 5 ["cubed"] -> Nothing
-- - "What is 1 plus multiplied by 2?" (operator followed by operator part) -> parseExpr (1+?) ["multiplied", "by", "2"] -> parseNumAndApply fails on readMaybe "multiplied"
parseExpr _ _ = Nothing

-- | Helper function to read the number string that follows an operator,
-- apply the operation, and recurse into parseExpr with the updated accumulator
-- and the rest of the tokens.
-- Returns Nothing if the number string is not a valid integer.
parseNumAndApply :: Integer -> (Integer -> Integer -> Integer) -> String -> [String] -> Maybe Integer
parseNumAndApply acc op numStr rest = do
    num <- readMaybe numStr -- Fails (returns Nothing) if numStr is not a valid Integer
    -- Note: Standard integer division `div` will raise a runtime error on division by zero.
    -- The problem description doesn't specify handling this edge case explicitly.
    -- If required, a check could be added here: `if op == div && num == 0 then Nothing else ...`
    parseExpr (acc `op` num) rest
