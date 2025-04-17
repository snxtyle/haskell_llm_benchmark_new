module WordProblem (answer) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Control.Monad (guard)

data Op = Add | Subtract | Multiply | Divide

answer :: String -> Maybe Integer
answer input = do
  -- Trim input and ensure it ends with a question mark
  let t = trim input
  guard (not (null t) && last t == '?')
  let body = trim (init t)
  -- Ensure it starts with "What is "
  let prefix = "What is "
  guard (prefix `isPrefixOf` body)
  let rest = drop (length prefix) body
  -- Parse the expression
  (n, ops) <- parseExpr (words rest)
  -- Evaluate left-to-right
  return (evaluate n ops)

-- Evaluate list of operations left to right
evaluate :: Integer -> [(Op, Integer)] -> Integer
evaluate = foldl apply
  where
    apply acc (op, n) =
      case op of
        Add      -> acc + n
        Subtract -> acc - n
        Multiply -> acc * n
        Divide   -> acc `div` n

-- Parse expression into initial number and list of operations
parseExpr :: [String] -> Maybe (Integer, [(Op, Integer)])
parseExpr (x:xs) = do
  n   <- readNumber x
  ops <- parseRest xs
  return (n, ops)
parseExpr _ = Nothing

-- Parse remaining tokens into operations
parseRest :: [String] -> Maybe [(Op, Integer)]
parseRest [] = Just []
parseRest ("plus":y:ys) = do
  n    <- readNumber y
  rest <- parseRest ys
  return ((Add, n) : rest)
parseRest ("minus":y:ys) = do
  n    <- readNumber y
  rest <- parseRest ys
  return ((Subtract, n) : rest)
parseRest ("multiplied":"by":y:ys) = do
  n    <- readNumber y
  rest <- parseRest ys
  return ((Multiply, n) : rest)
parseRest ("divided":"by":y:ys) = do
  n    <- readNumber y
  rest <- parseRest ys
  return ((Divide, n) : rest)
parseRest _ = Nothing

-- Read an integer, including negative
readNumber :: String -> Maybe Integer
readNumber = readMaybe

-- Trim whitespace from both ends
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse
