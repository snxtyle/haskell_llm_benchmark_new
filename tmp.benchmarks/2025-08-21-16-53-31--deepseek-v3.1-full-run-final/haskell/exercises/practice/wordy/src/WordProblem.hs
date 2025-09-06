module WordProblem (answer) where

import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (listToMaybe)

answer :: String -> Maybe Integer
answer question
  | not (validQuestion question) = Nothing
  | otherwise = parseAndEvaluate (extractExpression question)

validQuestion :: String -> Bool
validQuestion s = "What is" `isPrefixOf` s && "?" `isSuffixOf` s

extractExpression :: String -> String
extractExpression s = 
  let withoutQuestion = drop (length "What is") s
      withoutQuestionMark = take (length withoutQuestion - 1) withoutQuestion
  in withoutQuestionMark

parseAndEvaluate :: String -> Maybe Integer
parseAndEvaluate expr = case parseTokens (words expr) of
  Just tokens -> evaluate tokens
  Nothing -> Nothing

data Token = Number Integer | Plus | Minus | Times | Divide
  deriving (Show, Eq)

parseTokens :: [String] -> Maybe [Token]
parseTokens [] = Nothing
parseTokens ws = parseTokens' ws []
  where
    parseTokens' [] acc = Just (reverse acc)
    parseTokens' (w:rest) acc
      | all (\c -> isDigit c || c == '-') w = 
          case readMaybe w of
            Just n -> parseTokens' rest (Number n : acc)
            Nothing -> Nothing
      | w == "plus" = parseTokens' rest (Plus : acc)
      | w == "minus" = parseTokens' rest (Minus : acc)
      | w == "multiplied" && not (null rest) && head rest == "by" = 
          parseTokens' (tail rest) (Times : acc)
      | w == "divided" && not (null rest) && head rest == "by" = 
          parseTokens' (tail rest) (Divide : acc)
      | otherwise = Nothing

readMaybe :: String -> Maybe Integer
readMaybe s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

evaluate :: [Token] -> Maybe Integer
evaluate [] = Nothing
evaluate [Number x] = Just x
evaluate (Number x : op : Number y : rest) = 
  case applyOp op x y of
    Just result -> evaluate (Number result : rest)
    Nothing -> Nothing
evaluate _ = Nothing

applyOp :: Token -> Integer -> Integer -> Maybe Integer
applyOp Plus x y = Just (x + y)
applyOp Minus x y = Just (x - y)
applyOp Times x y = Just (x * y)
applyOp Divide x y
  | y == 0 = Nothing
  | otherwise = Just (x `div` y)
applyOp _ _ _ = Nothing
