module WordProblem (answer) where

import Text.Parsec
import Text.Parsec.String (Parser)

answer :: String -> Maybe Integer
answer problem = case parse parseProblem "" problem of
    Left _  -> Nothing
    Right n -> Just n

-- Parser for the word problem
parseProblem :: Parser Integer
parseProblem = do
    _ <- string "What is "
    result <- parseExpression
    _ <- char '?'
    return result

-- Parser for expressions (handles multiple operations left-to-right)
parseExpression :: Parser Integer
parseExpression = do
    firstNum <- parseNumber
    operations <- many parseOperation
    return $ foldl (flip ($)) firstNum operations

-- Parser for a single operation and the number that follows it
parseOperation :: Parser (Integer -> Integer)
parseOperation = do
    _ <- spaces
    op <- choice
        [ string "plus" >> return (+)
        , string "minus" >> return (-)
        , string "multiplied by" >> return (*)
        , string "divided by" >> return div
        ]
    _ <- spaces
    num <- parseNumber
    return $ \x -> op x num

-- Parser for a number (handles positive and negative integers)
parseNumber :: Parser Integer
parseNumber = do
    _ <- spaces
    sign <- option 1 (char '-' >> return (-1))
    digits <- many1 digit
    return $ sign * read digits
