module Phone (number) where

import Data.Char (isDigit)

-- | Cleans and validates a NANP phone number string.
-- Returns Just the 10-digit number string if valid, otherwise Nothing.
number :: String -> Maybe String
number xs =
  let digits = filter isDigit xs
  in case length digits of
       10 -> validate digits
       11 -> if head digits == '1'
             then validate (tail digits)
             else Nothing -- 11 digits but doesn't start with '1'
       _  -> Nothing -- Incorrect number of digits

-- | Validates a 10-digit number string according to NANP rules (NXX-NXX-XXXX).
validate :: String -> Maybe String
validate num =
  if length num == 10 && isValid (head num) && isValid (num !! 3)
  then Just num
  else Nothing
  where
    -- N must be a digit from 2 through 9
    isValid c = c >= '2' && c <= '9'
