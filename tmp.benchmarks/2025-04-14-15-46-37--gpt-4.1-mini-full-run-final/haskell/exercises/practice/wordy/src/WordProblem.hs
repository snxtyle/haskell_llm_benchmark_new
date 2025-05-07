module WordProblem (answer) where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isSpace)
import Data.Maybe (listToMaybe)

answer :: String -> Maybe Integer
answer problem = case readP_to_S parseProblem problem of
    [(result, "")] -> Just result
    _              -> Nothing

-- Parser for the entire problem
parseProblem :: ReadP Integer
parseProblem = do
    skipSpaces
    string "What is "
    skipSpaces
    result <- parseExpression
    skipSpaces
    char '?'
    skipSpaces
    eof
    return result

-- Parse the expression: a number followed by zero or more operations and numbers
parseExpression :: ReadP Integer
parseExpression = do
    n <- parseNumber
    parseOperations n

-- Parse zero or more operations applied left-to-right
parseOperations :: Integer -> ReadP Integer
parseOperations acc = (do
    skipSpaces
    op <- parseOperation
    skipSpaces
    n <- parseNumber
    let acc' = op acc n
    parseOperations acc') <++ return acc

-- Parse a number (possibly negative)
parseNumber :: ReadP Integer
parseNumber = do
    skipSpaces
    sign <- option '+' (char '-')
    digits <- munch1 isDigit
    let n = read digits
    return $ if sign == '-' then -n else n

-- Parse an operation word and return the corresponding function
parseOperation :: ReadP (Integer -> Integer -> Integer)
parseOperation = choice
    [ string "plus" >> return (+)
    , string "minus" >> return (-)
    , string "multiplied by" >> return (*)
    , string "divided by" >> return div
    ]
