module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

-- Doubles a digit for the Luhn calculation.
-- If doubling results in a number > 9, 9 is subtracted.
luhnDouble :: Int -> Int
luhnDouble d = let dbl = d * 2 in if dbl > 9 then dbl - 9 else dbl

isValid :: String -> Bool
isValid n
  | length cleaned <= 1 = False
  | not (all isDigit cleaned) = False
  | otherwise =
      let digits = map digitToInt cleaned
          -- To process from right-to-left as the algorithm requires,
          -- we reverse the list of digits.
          reversedDigits = reverse digits
          -- We apply a transformation to every second digit, starting from the right.
          -- `cycle [id, luhnDouble]` creates an infinite list of functions:
          -- [id, luhnDouble, id, luhnDouble, ...]
          -- `zipWith ($)` applies these functions to the reversed digits.
          -- The first digit from the right is unchanged (id).
          -- The second is transformed (luhnDouble).
          -- The third is unchanged, and so on.
          transformed = zipWith ($) (cycle [id, luhnDouble]) reversedDigits
          total = sum transformed
      in total `mod` 10 == 0
  where
    -- Remove spaces from the input string.
    cleaned = filter (/= ' ') n
