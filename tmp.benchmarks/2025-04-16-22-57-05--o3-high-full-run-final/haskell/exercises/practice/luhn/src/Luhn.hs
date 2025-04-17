module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

-- | Check whether a string represents a valid number according to the Luhn
-- algorithm.
--
-- The rules implemented are:
--   • Ignore spaces in the input.
--   • Any other non‑digit character makes the input invalid.
--   • After removing spaces, an input of length 1 or less is invalid.
--   • Apply the Luhn checksum to determine validity.
isValid :: String -> Bool
isValid input
  | length digits <= 1        = False
  | any (not . isDigit) digits = False
  | otherwise                 = checksum `mod` 10 == 0
  where
    -- Remove spaces; keep the rest of the characters for validation.
    digits = filter (/= ' ') input

    -- Convert the digit characters to Ints.
    digitValues :: [Int]
    digitValues = map digitToInt digits

    -- Apply the Luhn transformation starting from the right.
    -- Every second digit (index 1,3,…) in the reversed list is doubled;
    -- if the result exceeds 9 we subtract 9.
    luhnTransform :: [Int] -> [Int]
    luhnTransform = zipWith transform [0 ..]
      where
        transform :: Int -> Int -> Int
        transform idx d
          | idx `mod` 2 == 1 = let doubled = d * 2
                               in if doubled > 9 then doubled - 9 else doubled
          | otherwise        = d

    checksum :: Int
    checksum = sum . luhnTransform . reverse $ digitValues
