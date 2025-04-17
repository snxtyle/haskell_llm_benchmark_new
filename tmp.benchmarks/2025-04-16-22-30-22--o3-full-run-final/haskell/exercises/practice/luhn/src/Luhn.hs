module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

-- | Validate a number (represented as a 'String') using the Luhn algorithm.
--
-- The algorithm works as follows:
--
-- 1. Remove spaces from the input.
-- 2. Reject the input if its length is lower than 2 or if it contains
--    any non‑digit characters.
-- 3. Starting from the right‑most digit, double every second digit.
--    If doubling produces a number greater than 9, subtract 9.
-- 4. Sum all the digits and check that the total is divisible by 10.
isValid :: String -> Bool
isValid input
  | length digits <= 1          = False
  | any (not . isDigit) digits  = False
  | otherwise                   = total `mod` 10 == 0
  where
    -- Remove spaces
    digits = filter (/= ' ') input

    -- Luhn checksum
    total :: Int
    total = sum $ zipWith transform (reverse $ map digitToInt digits) [0 :: Int ..]

    -- Double every second digit from the right; subtract 9 if result > 9
    transform :: Int -> Int -> Int
    transform d idx
      | odd idx   = let dbl = d * 2 in if dbl > 9 then dbl - 9 else dbl
      | otherwise = d
