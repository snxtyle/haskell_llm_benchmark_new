module WordProblem (answer) where

import Data.Char (isSpace)
import Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer problem = do
  stripped <- stripQuestion problem
  let toks = words stripped
  (n0, rest) <- parseNumber toks
  result <- evalOps n0 rest
  case result of
    (val, []) -> Just val
    _         -> Nothing

-- Ensure question form "What is ...?"
stripQuestion :: String -> Maybe String
stripQuestion s =
  let trimmed = trim s
      prefix = "What is "
      suffix = "?"
  in if prefix `isPrefixOf` trimmed && suffix `isSuffixOf` trimmed
       then Just $ take (length trimmed - length suffix) (drop (length prefix) trimmed)
       else Nothing

-- Helpers: prefix/suffix checks without importing extra libs
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isSuffixOf :: String -> String -> Bool
isSuffixOf suf s = reverse suf `isPrefixOf` reverse s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = reverse . dropWhile p . reverse

-- Parse an Integer possibly signed
parseNumber :: [String] -> Maybe (Integer, [String])
parseNumber [] = Nothing
parseNumber (t:ts) =
  case readMaybeInteger t of
    Just n  -> Just (n, ts)
    Nothing -> Nothing

readMaybeInteger :: String -> Maybe Integer
readMaybeInteger str = readMaybe str :: Maybe Integer

-- Evaluate a sequence of op-number tokens left-to-right
-- Returns (value, remainingTokens) to allow final-consumption check
evalOps :: Integer -> [String] -> Maybe (Integer, [String])
evalOps acc [] = Just (acc, [])
evalOps acc toks = do
  (op, rest1) <- parseOp toks
  (n, rest2)  <- parseNumber rest1
  acc' <- applyOp op acc n
  evalOps acc' rest2

data Op = Add | Sub | Mul | Div deriving (Eq, Show)

applyOp :: Op -> Integer -> Integer -> Maybe Integer
applyOp Add  a b = Just (a + b)
applyOp Sub  a b = Just (a - b)
applyOp Mul  a b = Just (a * b)
applyOp Div  _  0 = Nothing
applyOp Div  a  b = Just (a `div` b)

-- Parse an operation token. Supported:
-- "plus", "minus", "multiplied by", "divided by"
parseOp :: [String] -> Maybe (Op, [String])
parseOp ("plus":rest) = Just (Add, rest)
parseOp ("minus":rest) = Just (Sub, rest)
parseOp ("multiplied":"by":rest) = Just (Mul, rest)
parseOp ("divided":"by":rest) = Just (Div, rest)
parseOp _ = Nothing
