module Phone (number) where

import Data.Char (isDigit, isSpace)
import Data.List (elemIndex)

number :: String -> Maybe String
number xs
  | not (all allowedChar xs) = Nothing
  | not (validPlus xs) = Nothing
  | not (validParentheses xs) = Nothing
  | otherwise = do
      ds <- normalizeDigits xs
      if validNANP ds then Just ds else Nothing

-- Allow digits, whitespace, and common phone punctuation.
allowedChar :: Char -> Bool
allowedChar c = isDigit c || isSpace c || c `elem` "()+-."

-- '+' is allowed only once and only as the first non-space character.
validPlus :: String -> Bool
validPlus s =
  case dropWhile isSpace s of
    ('+':rest) -> '+' `notElem` rest
    _          -> '+' `notElem` s

-- Parentheses must be balanced. If present, exactly one pair enclosing exactly
-- three digits (the area code) is allowed.
validParentheses :: String -> Bool
validParentheses s =
  case (opens, closes) of
    (0, 0) -> True
    (1, 1) ->
      case (elemIndex '(' s, elemIndex ')' s) of
        (Just i, Just j) ->
          i < j && countDigits (take (j - i - 1) (drop (i + 1) s)) == 3
        _ -> False
    _ -> False
  where
    opens = length (filter (=='(') s)
    closes = length (filter (==')') s)
    countDigits = length . filter isDigit

-- Extract digits and normalize:
-- - 10 digits => keep
-- - 11 digits with leading '1' => drop the leading '1'
-- - otherwise invalid
normalizeDigits :: String -> Maybe String
normalizeDigits s =
  case filter isDigit s of
    ds | length ds == 10 -> Just ds
    '1':ds | length ds == 10 -> Just ds
    _ -> Nothing

-- Enforce NANP rules: NXX NXX-XXXX, where N ∈ [2..9].
validNANP :: String -> Bool
validNANP ds =
  length ds == 10
    && nOk (ds !! 0)  -- area code first digit
    && nOk (ds !! 3)  -- exchange code first digit
  where
    nOk c = c >= '2' && c <= '9'
