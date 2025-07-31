module WordProblem (answer) where

import Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer problem = parseProblem problem

parseProblem :: String -> Maybe Integer
parseProblem problem
  | "What is " `isPrefixOf` problem && "?" `isSuffixOf` trimmed =
      evaluateExpression (init (drop 8 trimmed))
  | otherwise = Nothing
  where
    trimmed = trim problem

-- Evaluate expression from left to right
evaluateExpression :: String -> Maybe Integer
evaluateExpression expr = parseExpression (words expr)

parseExpression :: [String] -> Maybe Integer
parseExpression [] = Nothing
parseExpression (first:rest) = do
  num <- readMaybe first
  parseOperations num rest

parseOperations :: Integer -> [String] -> Maybe Integer
parseOperations acc [] = Just acc
parseOperations acc (word:rest) = case word of
  "plus" -> case rest of
    (next:remaining) -> do
      nextNum <- readMaybe next
      parseOperations (acc + nextNum) remaining
    [] -> Nothing
  "minus" -> case rest of
    (next:remaining) -> do
      nextNum <- readMaybe next
      parseOperations (acc - nextNum) remaining
    [] -> Nothing
  "multiplied" -> case rest of
    ("by":next:remaining) -> do
      nextNum <- readMaybe next
      parseOperations (acc * nextNum) remaining
    _ -> Nothing
  "divided" -> case rest of
    ("by":next:remaining) -> do
      nextNum <- readMaybe next
      if nextNum == 0
        then Nothing
        else parseOperations (acc `div` nextNum) remaining
    _ -> Nothing
  _ -> Nothing  -- Unsupported operation

-- Helper functions
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys)
  | x == y = isPrefixOf xs ys
  | otherwise = False

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix str = reverse suffix `isPrefixOf` reverse str

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')
