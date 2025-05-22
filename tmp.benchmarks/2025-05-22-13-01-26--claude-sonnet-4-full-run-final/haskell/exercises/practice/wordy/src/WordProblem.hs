module WordProblem (answer) where

import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)

data Operation = Plus | Minus | Times | DividedBy
  deriving (Show, Eq)

answer :: String -> Maybe Integer
answer problem = parseAndEvaluate (words (map toLowerIfAlpha problem))
  where
    toLowerIfAlpha c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

parseAndEvaluate :: [String] -> Maybe Integer
parseAndEvaluate tokens = do
  cleanTokens <- removeQuestionMark tokens
  case cleanTokens of
    ("what":"is":rest) -> parseExpression rest
    _ -> Nothing

removeQuestionMark :: [String] -> Maybe [String]
removeQuestionMark [] = Just []
removeQuestionMark tokens = 
  let lastToken = last tokens
      initTokens = init tokens
  in if '?' `elem` lastToken
     then Just (initTokens ++ [filter (/= '?') lastToken | filter (/= '?') lastToken /= ""])
     else Just tokens

parseExpression :: [String] -> Maybe Integer
parseExpression [] = Nothing
parseExpression [numStr] = parseNumber numStr
parseExpression (numStr:rest) = do
  firstNum <- parseNumber numStr
  evaluateOperations firstNum rest

evaluateOperations :: Integer -> [String] -> Maybe Integer
evaluateOperations acc [] = Just acc
evaluateOperations acc tokens = do
  (op, numStr, remaining) <- parseOperation tokens
  num <- parseNumber numStr
  newAcc <- applyOperation acc op num
  evaluateOperations newAcc remaining

parseOperation :: [String] -> Maybe (Operation, String, [String])
parseOperation [] = Nothing
parseOperation [_] = Nothing
parseOperation (opStr:numStr:rest) = do
  op <- parseOperator opStr
  Just (op, numStr, rest)
parseOperation ("multiplied":"by":numStr:rest) = Just (Times, numStr, rest)
parseOperation ("divided":"by":numStr:rest) = Just (DividedBy, numStr, rest)
parseOperation _ = Nothing

parseOperator :: String -> Maybe Operation
parseOperator "plus" = Just Plus
parseOperator "minus" = Just Minus
parseOperator "multiplied" = Nothing  -- handled specially
parseOperator "divided" = Nothing     -- handled specially
parseOperator _ = Nothing

parseNumber :: String -> Maybe Integer
parseNumber str = 
  case str of
    ('-':rest) -> do
      num <- readMaybe rest
      Just (-num)
    _ -> readMaybe str

applyOperation :: Integer -> Operation -> Integer -> Maybe Integer
applyOperation x Plus y = Just (x + y)
applyOperation x Minus y = Just (x - y)
applyOperation x Times y = Just (x * y)
applyOperation x DividedBy y = 
  if y == 0 then Nothing else Just (x `div` y)
