module Hexadecimal (hexToInt) where

import Data.Char (toLower)
import Data.Traversable (traverse) -- Correct module for 'traverse'

-- | Converts a single hexadecimal character to its integer value (0-15).
-- Returns Nothing if the character is not a valid hex digit.
charToDigit :: Char -> Maybe Int
charToDigit c
  | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
  | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
  | otherwise            = Nothing

-- | Converts a hexadecimal string to its decimal integer equivalent.
-- Returns 0 if the string contains any invalid hexadecimal characters.
hexToInt :: String -> Int
hexToInt hexStr =
  -- Convert the input string to lowercase to handle 'A'-'F' and 'a'-'f' uniformly.
  -- Then, attempt to convert each character to a digit using charToDigit.
  -- 'traverse' will return 'Nothing' if any charToDigit returns 'Nothing'.
  case traverse charToDigit (map toLower hexStr) of
    Just digits ->
      -- If all characters are valid, fold over the list of digits to calculate the decimal value.
      -- For each digit, multiply the accumulated value by 16 and add the current digit.
      foldl (\acc digit -> acc * 16 + digit) 0 digits
    Nothing     ->
      -- If any character was invalid, return 0 as per the problem's handling of invalid strings.
      0
