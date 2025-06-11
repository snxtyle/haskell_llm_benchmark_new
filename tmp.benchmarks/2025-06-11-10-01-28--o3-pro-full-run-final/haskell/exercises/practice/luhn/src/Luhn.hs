module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

-- | Determine if a string represents a valid number per the Luhn algorithm.
--
-- Rules implemented:
--   * Input of length 1 or less is invalid.
--   * Spaces are ignored.
--   * Any character other than spaces and decimal digits invalidates the input.
--   * Applies the Luhn checksum to the cleaned digits.
isValid :: String -> Bool
isValid input =
    let stripped = filter (/= ' ') input
    in  length stripped > 1
        && all isDigit stripped
        && luhnSum stripped `mod` 10 == 0

-- | Compute the Luhn checksum of a string consisting solely of digits.
luhnSum :: String -> Int
luhnSum s =
    let digitsReversed = map digitToInt (reverse s)
        transform idx d
          | odd idx   = let doubled = d * 2
                        in if doubled > 9 then doubled - 9 else doubled
          | otherwise = d
    in  sum $ zipWith transform [0..] digitsReversed
