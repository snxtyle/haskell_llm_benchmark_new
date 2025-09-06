module WordProblem (answer) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<|>))

answer :: String -> Maybe Integer
answer problem = case parse wordProblem "" problem of
  Left _ -> Nothing
  Right result -> Just result

-- Main parser for the word problem
wordProblem :: Parser Integer
wordProblem = do
  string "What is "
  result <- number
  rest <- many operation
  char '?'
  eof
  return $ foldl (flip ($)) result (reverse rest)

-- Parse a number (positive or negative)
number :: Parser Integer
number = do
  spaces
  sign <- option '+' (char '+' <|> char '-')
  spaces
  digits <- many1 digit
  let n = read digits
  return $ if sign == '-' then -n else n

-- Parse an operation
operation :: Parser (Integer -> Integer)
operation = do
  spaces
  op <- plus <|> minus <|> multiply <|> divide
  spaces
  n <- number
  return (op n)

-- Addition
plus :: Parser (Integer -> Integer)
plus = string "plus" >> return (+)

-- Subtraction
minus :: Parser (Integer -> Integer)
minus = string "minus" >> return (-)

-- Multiplication
multiply :: Parser (Integer -> Integer)
multiply = try (string "multiplied by") >> return (*)

-- Division
divide :: Parser (Integer -> Integer)
divide = try (string "divided by") >> return div
