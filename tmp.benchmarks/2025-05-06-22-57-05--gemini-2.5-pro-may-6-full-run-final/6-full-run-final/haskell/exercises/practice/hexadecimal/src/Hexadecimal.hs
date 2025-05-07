module Hexadecimal (hexToInt) where

import Data.Char (isHexDigit, digitToInt)

-- | Converts a hexadecimal string to its decimal integer equivalent.
--
--   - Valid hexadecimal characters are '0'-'9', 'a'-'f', and 'A'-'F'.
--   - The conversion is case-insensitive.
--   - If the input string contains any invalid hexadecimal characters,
--     the function returns 0.
--   - An empty string is also considered invalid (in the sense that it doesn't represent
--     a number other than 0 via this function's logic) and results in 0.
--   - The conversion is done from first principles using a fold.
--
--   Examples:
--   hexToInt "10af8c" == 1093516
--   hexToInt "008000" == 32768
--   hexToInt "0"      == 0
--   hexToInt ""       == 0
--   hexToInt "invalid" == 0
--   hexToInt "1g0"    == 0
hexToInt :: String -> Int
hexToInt hexStr
  -- Check if all characters in the string are valid hexadecimal digits.
  -- The `all` function returns True for an empty list/string,
  -- so `all isHexDigit ""` is True.
  | all isHexDigit hexStr =
      -- If all characters are valid (or the string is empty),
      -- perform the conversion using a left fold.
      -- `foldl` on an empty string with initial value 0 will return 0.
      foldl step 0 hexStr
  | otherwise =
      -- If any character is not a valid hex digit, return 0.
      0
  where
    -- The `step` function for the fold:
    -- accumulator is the current decimal value.
    -- hexChar is the next hexadecimal character to process.
    -- `digitToInt` converts a hex character (e.g., 'a') to its integer value (e.g., 10).
    step accumulator hexChar = accumulator * 16 + digitToInt hexChar
