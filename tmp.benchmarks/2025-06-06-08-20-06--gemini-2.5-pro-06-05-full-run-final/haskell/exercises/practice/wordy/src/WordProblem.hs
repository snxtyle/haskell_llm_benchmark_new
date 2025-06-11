module WordProblem (answer) where

import Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer problem =
    let ws = words . filter (/= '?') $ problem
    in case ws of
        ("What":"is":rest) -> parseExpr rest
        _                  -> Nothing

parseExpr :: [String] -> Maybe Integer
parseExpr [] = Nothing
parseExpr (nStr:rest) = do
    n <- readMaybe nStr
    eval rest n

eval :: [String] -> Integer -> Maybe Integer
eval [] acc = Just acc
eval ("plus":nStr:rest) acc = do
    n <- readMaybe nStr
    eval rest (acc + n)
eval ("minus":nStr:rest) acc = do
    n <- readMaybe nStr
    eval rest (acc - n)
eval ("multiplied":"by":nStr:rest) acc = do
    n <- readMaybe nStr
    eval rest (acc * n)
eval ("divided":"by":nStr:rest) acc = do
    n <- readMaybe nStr
    eval rest (acc `div` n)
eval _ _ = Nothing
