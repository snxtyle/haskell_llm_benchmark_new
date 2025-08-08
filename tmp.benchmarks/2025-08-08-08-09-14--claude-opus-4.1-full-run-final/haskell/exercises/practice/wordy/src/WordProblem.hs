module WordProblem (answer) where

import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)

answer :: String -> Maybe Integer
answer problem = parseWordProblem problem

-- Main parser for word problems
parseWordProblem :: String -> Maybe Integer
parseWordProblem input = do
  -- Check if it starts with "What is "
  rest1 <- stripPrefix "What is " input
  -- Check if it ends with "?"
  let trimmed = dropWhileEnd isSpace rest1
  if not (null trimmed) && last trimmed == '?'
    then do
      let withoutQuestion = init trimmed
      -- Parse the expression
      parseExpression withoutQuestion
    else Nothing

-- Parse an expression (number followed by zero or more operations)
parseExpression :: String -> Maybe Integer
parseExpression input = do
  let trimmed = dropWhile isSpace input
  (num, rest) <- parseNumber trimmed
  evaluateOperations num rest

-- Evaluate operations left-to-right
evaluateOperations :: Integer -> String -> Maybe Integer
evaluateOperations acc input = 
  let trimmed = dropWhile isSpace input
  in if null trimmed
     then Just acc
     else do
       (op, rest1) <- parseOperator trimmed
       (num, rest2) <- parseNumber rest1
       evaluateOperations (op acc num) rest2

-- Parse a number (positive or negative)
parseNumber :: String -> Maybe (Integer, String)
parseNumber input = 
  let trimmed = dropWhile isSpace input
  in case trimmed of
    [] -> Nothing
    ('-':rest) -> do
      (num, remaining) <- parsePositiveNumber rest
      Just (negate num, remaining)
    _ -> parsePositiveNumber trimmed

-- Parse a positive number
parsePositiveNumber :: String -> Maybe (Integer, String)
parsePositiveNumber input = 
  let digits = takeWhile isDigit input
      rest = dropWhile isDigit input
  in if null digits
     then Nothing
     else Just (read digits, rest)

-- Parse an operator
parseOperator :: String -> Maybe (Integer -> Integer -> Integer, String)
parseOperator input = 
  let trimmed = dropWhile isSpace input
  in tryOperator "plus" (+) trimmed <|>
     tryOperator "minus" (-) trimmed <|>
     tryOperator "multiplied by" (*) trimmed <|>
     tryOperator "divided by" div trimmed

-- Try to match an operator string
tryOperator :: String -> (Integer -> Integer -> Integer) -> String -> Maybe (Integer -> Integer -> Integer, String)
tryOperator opStr op input = 
  stripPrefix opStr input >>= \rest -> Just (op, rest)

-- Helper function similar to Data.List.stripPrefix but returns Maybe
stripPrefix :: String -> String -> Maybe String
stripPrefix prefix str = 
  if isPrefixOf prefix str
  then Just (drop (length prefix) str)
  else Nothing

-- Helper function similar to Data.List.dropWhileEnd
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

-- Alternative operator (similar to <|> in parsers)
(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> b = b
a <|> _ = a
