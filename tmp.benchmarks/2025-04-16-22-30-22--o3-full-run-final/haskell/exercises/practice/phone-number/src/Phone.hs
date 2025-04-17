module Phone (number) where

import Data.Char (isDigit, isAlpha)

-- | Clean up a phone number according to the North American Numbering Plan (NANP)
--   Rules implemented:
--
--   * All letters make the number invalid.
--   * Punctuation characters are ignored.
--   * After stripping non‑digit characters we must have either:
--       - 10 digits, or
--       - 11 digits whose first digit is '1'. In the latter case the leading
--         '1' (country code) is removed.
--   * The resulting 10‑digit number must satisfy:
--       - Area code (first digit) is not 0 or 1.
--       - Exchange code (fourth digit) is not 0 or 1.
number :: String -> Maybe String
number xs
  | any isAlpha xs = Nothing           -- letters are not allowed
  | otherwise      = validate cleanedDigits
  where
    -- keep only the digits found in the input
    cleanedDigits = filter isDigit xs

    validate ds
      | length ds == 10                         = checkRules ds
      | length ds == 11 && head ds == '1'       = checkRules (tail ds)
      | otherwise                               = Nothing

    -- Ensure area code and exchange code start with 2‑9
    checkRules ds
      | length ds /= 10        = Nothing
      | ds !! 0 `elem` "01"    = Nothing   -- area code cannot start with 0 or 1
      | ds !! 3 `elem` "01"    = Nothing   -- exchange code cannot start with 0 or 1
      | otherwise              = Just ds
