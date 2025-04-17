module WordProblem (answer) where

import Data.Char  (isSpace)
import Data.List  (stripPrefix)
import Text.Read  (readMaybe)

-- | Public API ---------------------------------------------------------------

-- | Parse a verbal arithmetic problem and evaluate it.
--   Returns 'Nothing' when the problem is not understood or is malformed.
answer :: String -> Maybe Integer
answer input = do
  inner <- stripEnvelope input                -- remove leading "What is " … "?"
  tokens <- pure (words inner)                -- tokenise on spaces
  (firstNumber, rest) <- parseFirstNumber tokens
  evalSequence firstNumber rest

-- | Helpers ------------------------------------------------------------------

-- Expect the problem to start with "What is " and end with a single "?".
stripEnvelope :: String -> Maybe String
stripEnvelope s = do
  body <- stripPrefix "What is " (trimStart s) -- must start with “What is ”
  if null body || last body /= '?'
     then Nothing
     else Just . trim . init $ body            -- drop trailing '?' and trim
  where
    trimStart = dropWhile isSpace
    trim      = f . f
      where f = reverse . dropWhile isSpace

-- Parse the very first number in the token list.
parseFirstNumber :: [String] -> Maybe (Integer, [String])
parseFirstNumber []       = Nothing
parseFirstNumber (t:rest) = do
  n <- parseNum t
  Just (n, rest)

-- Evaluate the remaining sequence of (operator, number) pairs, left‑to‑right.
evalSequence :: Integer -> [String] -> Maybe Integer
evalSequence acc [] = Just acc
evalSequence acc (op:ts) =
  case op of
    "plus"       -> binaryOp (+) ts
    "minus"      -> binaryOp (-) ts
    "multiplied" -> expect "by" ts >>= binaryOp (*) . snd
    "divided"    -> expect "by" ts >>= binaryOpSafeDiv . snd
    _            -> Nothing
  where
    -- Expect next token to match the given string.
    expect w (x:xs)
      | x == w    = Just (x, xs)
      | otherwise = Nothing
    expect _ _    = Nothing

    -- Apply a binary operator that is always safe.
    binaryOp f (nTok:rest') = do
      n <- parseNum nTok
      evalSequence (f acc n) rest'
    binaryOp _ _            = Nothing

    -- Division with zero‑check.
    binaryOpSafeDiv (nTok:rest') = do
      n <- parseNum nTok
      if n == 0
         then Nothing
         else evalSequence (acc `div` n) rest'
    binaryOpSafeDiv _             = Nothing

-- Try to read an integral number (handles negatives as well).
parseNum :: String -> Maybe Integer
parseNum = readMaybe
