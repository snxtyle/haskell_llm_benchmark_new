module Phone (number) where

import Data.Char (isDigit)

-- | Cleans a phone number string and validates it according to NANP rules.
-- Returns Just the 10-digit number string if valid, otherwise Nothing.
number :: String -> Maybe String
number xs =
  let digits = filter isDigit xs
  in case length digits of
       10 -> validateNANP digits
       11 -> if head digits == '1'
             then validateNANP (tail digits)
             else Nothing
       _  -> Nothing

-- | Validates if a 10-digit string conforms to NANP rules.
-- NANP rules: NXX-NXX-XXXX where N is 2-9, X is 0-9.
-- This means the 1st digit (index 0) and 4th digit (index 3) must be N.
validateNANP :: String -> Maybe String
validateNANP numStr =
  if length numStr == 10 &&         -- Must be 10 digits
     isN (numStr !! 0) &&           -- Area code's first digit (N)
     isN (numStr !! 3)              -- Exchange code's first digit (N)
  then Just numStr
  else Nothing

-- | Checks if a character is a digit from '2' through '9' (an 'N' digit in NANP).
isN :: Char -> Bool
isN c = c >= '2' && c <= '9'
