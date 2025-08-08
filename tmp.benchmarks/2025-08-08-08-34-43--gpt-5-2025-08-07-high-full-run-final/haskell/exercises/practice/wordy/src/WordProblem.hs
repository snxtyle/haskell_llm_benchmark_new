module WordProblem (answer) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer s = do
  rest <- stripPrefix "What is " s
  let rest' = dropWhile isSpace rest
  body <- stripQuestionMark rest'
  parseExpression body

-- Remove trailing spaces and a single trailing question mark.
stripQuestionMark :: String -> Maybe String
stripQuestionMark s =
  let s' = rstrip s
  in case s' of
       [] -> Nothing
       _  -> if last s' == '?' then Just (init s') else Nothing

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

parseExpression :: String -> Maybe Integer
parseExpression body =
  case words body of
    []      -> Nothing
    (t:ts)  -> do
      n <- readInt t
      parseOps n ts

parseOps :: Integer -> [String] -> Maybe Integer
parseOps acc [] = Just acc
parseOps acc ("plus":t:rest) = do
  n <- readInt t
  parseOps (acc + n) rest
parseOps acc ("minus":t:rest) = do
  n <- readInt t
  parseOps (acc - n) rest
parseOps acc ("multiplied":"by":t:rest) = do
  n <- readInt t
  parseOps (acc * n) rest
parseOps acc ("divided":"by":t:rest) = do
  n <- readInt t
  if n == 0
     then Nothing
     else parseOps (acc `div` n) rest
parseOps _ _ = Nothing

readInt :: String -> Maybe Integer
readInt = readMaybe
