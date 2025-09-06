module WordProblem (answer) where

import Text.ParserCombinators.ReadP
import Control.Monad (foldM)
import Data.Char (isDigit)

data Expr = Num Integer | Plus Integer | Minus Integer | Mul Integer | Div Integer

answer :: String -> Maybe Integer
answer input = case readP_to_S parseProblem input of
  [(exprs, "")] -> evaluate exprs
  _ -> Nothing

parseProblem :: ReadP [Expr]
parseProblem = do
  _ <- string "What is"
  skipSpaces
  num <- parseNumber
  ops <- many parseOp
  skipSpaces
  optional (char '?')
  eof
  return (Num num : ops)

parseNumber :: ReadP Integer
parseNumber = do
  skipSpaces
  s <- option "" (string "-")
  digits <- munch1 isDigit
  skipSpaces
  return $ read (s ++ digits)

parseOp :: ReadP Expr
parseOp = do
  skipSpaces
  choice [ string "plus" >> skipSpaces >> parseNumber >>= return . Plus
         , string "minus" >> skipSpaces >> parseNumber >>= return . Minus
         , string "multiplied by" >> skipSpaces >> parseNumber >>= return . Mul
         , string "divided by" >> skipSpaces >> parseNumber >>= return . Div
         ]

evaluate :: [Expr] -> Maybe Integer
evaluate (Num n : rest) = foldM apply n rest
  where apply acc (Plus m) = Just (acc + m)
        apply acc (Minus m) = Just (acc - m)
        apply acc (Mul m) = Just (acc * m)
        apply acc (Div m) = if m == 0 then Nothing else Just (acc `div` m)
        apply acc (Num _) = Nothing
evaluate _ = Nothing
