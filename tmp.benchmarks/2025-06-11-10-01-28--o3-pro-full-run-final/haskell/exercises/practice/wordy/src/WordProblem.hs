module WordProblem (answer) where

import Control.Monad       (guard)
import Data.Char           (isSpace)
import Data.List           (isPrefixOf, isSuffixOf)
import Text.Read           (readMaybe)

-- | Trim leading and trailing white-space.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

-- | Attempt to solve a very small subset of natural-language word problems.
--
-- The grammar supported is:
--
--   problem := "What is " expression "?"
--   expression := number (operation number)*
--   operation  := "plus" | "minus" | "multiplied by" | "divided by"
--   number     := (optional '-') digit+
--
-- Evaluation is strictly left-to-right and stops with the first parse error,
-- returning Nothing.
answer :: String -> Maybe Integer
answer problem = do
  let stripped = trim problem
  guard ("What is " `isPrefixOf` stripped)
  guard ("?"        `isSuffixOf` stripped)

  -- remove the leading "What is " and the trailing '?'
  let inner  = trim $ init $ drop (length ("What is " :: String)) stripped
      tokens = words inner

  (firstTok : restTok) <- pure tokens
  firstNum             <- parseNumber firstTok
  eval firstNum restTok
  where
    -- Evaluate the remaining token stream left-to-right.
    eval :: Integer -> [String] -> Maybe Integer
    eval acc [] = Just acc
    eval acc ("plus" : xs) = do
      (n, rest) <- consumeNumber xs
      eval (acc + n) rest
    eval acc ("minus" : xs) = do
      (n, rest) <- consumeNumber xs
      eval (acc - n) rest
    eval acc ("multiplied" : "by" : xs) = do
      (n, rest) <- consumeNumber xs
      eval (acc * n) rest
    eval acc ("divided" : "by" : xs) = do
      (n, rest) <- consumeNumber xs
      guard (n /= 0)
      eval (acc `div` n) rest
    eval _ _ = Nothing

    consumeNumber :: [String] -> Maybe (Integer, [String])
    consumeNumber (t:ts) = do
      n <- parseNumber t
      return (n, ts)
    consumeNumber _ = Nothing

    parseNumber :: String -> Maybe Integer
    parseNumber = readMaybe
