module Phone (number) where

import Data.Char (isDigit)

-- | Clean and validate a NANP phone number.
-- Returns a 10-digit number as Just String if valid, otherwise Nothing.
number :: String -> Maybe String
number xs =
  let digits = filter isDigit xs
  in case normalize digits of
       Just ds | validNANP ds -> Just ds
       _ -> Nothing

-- Normalize by handling optional leading country code '1'.
-- Accept 10 digits as-is, or 11 digits starting with '1' (drop the '1').
-- Reject all other lengths.
normalize :: String -> Maybe String
normalize ds =
  case ds of
    -- 10-digit number
    d | length d == 10 -> Just d
    -- 11-digit number with leading country code '1'
    ('1':rest) | length rest == 10 -> Just rest
    -- Anything else is invalid
    _ -> Nothing

-- Validate NANP rules:
-- - Area code (first 3 digits) must start with 2-9.
-- - Exchange code (next 3 digits) must start with 2-9.
validNANP :: String -> Bool
validNANP ds =
  length ds == 10
    && headInRange (ds !! 0)
    && headInRange (ds !! 3)
  where
    headInRange c = c >= '2' && c <= '9'
