module WordProblem (answer) where

import Data.Char (isSpace)
import Data.List (find, isPrefixOf, stripPrefix)

-- | Evaluate a simple arithmetic word problem.
--   Returns Nothing for un‑parsable or unsupported questions.
answer :: String -> Maybe Integer
answer input = do
  body <- stripQuestion input           -- remove “What is ” … “?”
  (value, leftover) <- parseExpression body
  if all isSpace leftover               -- succeed only if everything was consumed
     then Just value
     else Nothing

-------------------------------------------------------------------------------
-- Helpers to preprocess the question

-- Ensure the sentence starts with the exact prefix and ends with '?',
-- then return the inner expression.
stripQuestion :: String -> Maybe String
stripQuestion str = do
  bodyWithQ <- stripPrefix "What is " str
  case unsnoc bodyWithQ of
    Just (body, '?') -> Just body
    _                -> Nothing
  where
    unsnoc [] = Nothing
    unsnoc xs = Just (init xs, last xs)

-------------------------------------------------------------------------------
-- Expression parser / evaluator (left‑to‑right, no operator precedence)

parseExpression :: String -> Maybe (Integer, String)
parseExpression s = do
  (initial, rest) <- parseNumber s
  go initial rest
  where
    go acc str =
      let trimmed = dropWhile isSpace str
      in if null trimmed
            then Just (acc, trimmed)
            else do
              (op, rest1)  <- parseOperation trimmed
              (val, rest2) <- parseNumber rest1
              go (op acc val) rest2

-- Parse a single signed integer.
parseNumber :: String -> Maybe (Integer, String)
parseNumber str =
  let trimmed = dropWhile isSpace str
  in case reads trimmed :: [(Integer, String)] of
       [(n, rest)] -> Just (n, rest)
       _           -> Nothing

-- Parse one of the supported operations.
parseOperation :: String -> Maybe (Integer -> Integer -> Integer, String)
parseOperation str = do
  let trimmed = dropWhile isSpace str
  (phrase, op) <- find matches operations
  rest <- stripPrefix (phrase ++ " ") trimmed
  return (op, rest)
  where
    operations =
      [ ("plus", (+))
      , ("minus", (-))
      , ("multiplied by", (*))
      , ("divided by", div)
      ]

    matches (ph, _) = (ph ++ " ") `isPrefixOf` dropWhile isSpace str
