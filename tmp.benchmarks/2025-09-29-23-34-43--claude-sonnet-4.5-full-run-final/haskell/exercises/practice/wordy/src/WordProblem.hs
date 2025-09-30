module WordProblem (answer) where

import Data.Char (isDigit, isSpace)
import Data.List (stripPrefix)

answer :: String -> Maybe Integer
answer problem = do
    -- Remove "What is " prefix and "?" suffix
    stripped <- stripPrefix "What is " problem
    let withoutQuestion = reverse . dropWhile isSpace . reverse $ stripped
    expr <- stripSuffix "?" withoutQuestion
    -- Parse and evaluate the expression
    parseAndEval (words expr)

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix str =
    let reversed = reverse str
        reversedSuffix = reverse suffix
    in fmap reverse (stripPrefix reversedSuffix reversed)

parseAndEval :: [String] -> Maybe Integer
parseAndEval [] = Nothing
parseAndEval ws = do
    (num, rest) <- parseNumber ws
    evalOps num rest

parseNumber :: [String] -> Maybe (Integer, [String])
parseNumber [] = Nothing
parseNumber (w:ws) = case readInteger w of
    Just n -> Just (n, ws)
    Nothing -> Nothing

readInteger :: String -> Maybe Integer
readInteger s
    | null s = Nothing
    | head s == '-' && length s > 1 && all isDigit (tail s) = Just (read s)
    | all isDigit s = Just (read s)
    | otherwise = Nothing

evalOps :: Integer -> [String] -> Maybe Integer
evalOps acc [] = Just acc
evalOps acc ws = do
    (op, rest1) <- parseOp ws
    (num, rest2) <- parseNumber rest1
    let newAcc = applyOp op acc num
    evalOps newAcc rest2

parseOp :: [String] -> Maybe (Op, [String])
parseOp [] = Nothing
parseOp ("plus":ws) = Just (Add, ws)
parseOp ("minus":ws) = Just (Sub, ws)
parseOp ("multiplied":"by":ws) = Just (Mul, ws)
parseOp ("divided":"by":ws) = Just (Div, ws)
parseOp _ = Nothing

data Op = Add | Sub | Mul | Div

applyOp :: Op -> Integer -> Integer -> Integer
applyOp Add x y = x + y
applyOp Sub x y = x - y
applyOp Mul x y = x * y
applyOp Div x y = x `div` y
