module WordProblem (answer) where

import Control.Monad (foldM)

data Op = Plus | Minus | Times | Div
data Token = Num Integer | Op' Op

answer :: String -> Maybe Integer
answer s
  | not (startsWith "What is " s && endsWithQ s) = Nothing
  | otherwise =
      let inner = safeInit (drop 8 s)
          tokens = tokenize $ words inner
      in tokens >>= toSteps >>= \(n, ops) -> evalSteps n ops

startsWith :: String -> String -> Bool
startsWith prefix str = take (length prefix) str == prefix

endsWithQ :: String -> Bool
endsWithQ s = not (null s) && last s == '?'

safeInit :: String -> String
safeInit s = if null s then s else init s

tokenize :: [String] -> Maybe [Token]
tokenize [] = Just []
tokenize (w:ws)
  | Just n <- readNumber w = (Num n :) <$> tokenize ws
  | w == "plus" = (Op' Plus :) <$> tokenize ws
  | w == "minus" = (Op' Minus :) <$> tokenize ws
  | w == "multiplied" && not (null ws) && head ws == "by" = (Op' Times :) <$> tokenize (tail ws)
  | w == "divided" && not (null ws) && head ws == "by" = (Op' Div :) <$> tokenize (tail ws)
  | otherwise = Nothing

readNumber :: String -> Maybe Integer
readNumber s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

toSteps :: [Token] -> Maybe (Integer, [(Op, Integer)])
toSteps (Num n : tokens) = 
  case parseSteps tokens of
    Just steps -> Just (n, steps)
    Nothing -> Nothing
toSteps _ = Nothing

parseSteps :: [Token] -> Maybe [(Op, Integer)]
parseSteps [] = Just []
parseSteps (Op' op : Num n : rest) = ((op, n) :) <$> parseSteps rest
parseSteps _ = Nothing

evalSteps :: Integer -> [(Op, Integer)] -> Maybe Integer
evalSteps start ops = foldM applyOp start ops

applyOp :: Integer -> (Op, Integer) -> Maybe Integer
applyOp x (Plus, y) = Just (x + y)
applyOp x (Minus, y) = Just (x - y)
applyOp x (Times, y) = Just (x * y)
applyOp x (Div, y)
  | y == 0 = Nothing
  | otherwise = Just (x `div` y)
