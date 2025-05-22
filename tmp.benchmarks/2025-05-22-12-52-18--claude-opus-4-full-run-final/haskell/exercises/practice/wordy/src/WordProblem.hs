module WordProblem (answer) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (isSpace)

data Operation = Add | Subtract | Multiply | Divide
  deriving (Show, Eq)

answer :: String -> Maybe Integer
answer problem = case parse wordProblemParser "" (trim problem) of
  Left _ -> Nothing
  Right result -> Just result
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

wordProblemParser :: Parser Integer
wordProblemParser = do
  _ <- string "What is "
  firstNum <- number
  operations <- many operationAndNumber
  _ <- char '?'
  _ <- eof
  return $ foldl applyOperation firstNum operations

number :: Parser Integer
number = do
  spaces
  sign <- optionMaybe (char '-')
  spaces
  digits <- many1 digit
  let num = read digits
  return $ case sign of
    Just '-' -> -num
    _ -> num

operationAndNumber :: Parser (Operation, Integer)
operationAndNumber = do
  spaces
  op <- operation
  spaces
  num <- number
  return (op, num)

operation :: Parser Operation
operation = choice
  [ string "plus" >> return Add
  , string "minus" >> return Subtract
  , string "multiplied by" >> return Multiply
  , string "divided by" >> return Divide
  ]

applyOperation :: Integer -> (Operation, Integer) -> Integer
applyOperation acc (Add, n) = acc + n
applyOperation acc (Subtract, n) = acc - n
applyOperation acc (Multiply, n) = acc * n
applyOperation acc (Divide, n) = acc `div` n
