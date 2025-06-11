module Phone (number) where

import Data.Char (isDigit, isAlpha)

-- | Clean up a NANP phone number.
--
-- The function returns `Just` the 10–digit number if it is valid,
-- otherwise it returns `Nothing`.
--
-- Validation rules (matching the Exercism “phone-number” exercise):
--
-- 1. After discarding punctuation, the input must contain either
--    • exactly 10 digits, or
--    • 11 digits where the first is the country-code ‘1’.
-- 2. When an 11-digit number starts with ‘1’, the leading ‘1’ is removed
--    and the remaining 10 digits are validated as below.
-- 3. The first digit of the area code (NXX) must be 2–9.
-- 4. The first digit of the exchange code (NXX) must be 2–9.
-- 5. If the input contains any alphabetic characters, it is invalid.
number :: String -> Maybe String
number xs
  | any isAlpha xs = Nothing                        -- reject letters
  | otherwise      = validate $ filter isDigit xs
  where
    validate :: String -> Maybe String
    -- 10-digit candidate
    validate ds
      | length ds == 10              = validateTen ds
      -- 11-digit candidate starting with ‘1’
      | length ds == 11, head ds == '1' = validateTen (tail ds)
      | otherwise                     = Nothing

    -- Validate NANP rules on a 10-digit string
    validateTen :: String -> Maybe String
    validateTen ds
      | length ds /= 10                              = Nothing
      | not (isAllowed (ds !! 0))                    = Nothing -- area code first digit
      | not (isAllowed (ds !! 3))                    = Nothing -- exchange code first digit
      | otherwise                                    = Just ds

    -- Digits 2–9 are allowed as the first digit in area/exchange codes
    isAllowed :: Char -> Bool
    isAllowed c = c `elem` ['2' .. '9']
