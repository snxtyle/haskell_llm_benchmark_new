module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

-- Helper function to double a digit and adjust if it's > 9
-- This is part of the Luhn algorithm for digits that are doubled.
doubleAndAdjust :: Int -> Int
doubleAndAdjust d =
  let doubled = d * 2
  in if doubled > 9 then doubled - 9 else doubled

isValid :: String -> Bool
isValid n =
  -- 1. Remove spaces from the input string.
  let cleanedN = filter (/= ' ') n
  in
    -- 2. Validate the cleaned string:
    --    - It must not be empty or have only one digit.
    --    - All characters must be digits.
    if length cleanedN <= 1 || not (all isDigit cleanedN)
    then False
    else
      -- 3. Convert character digits to integers.
      let digits = map digitToInt cleanedN
          -- 4. Reverse the list of digits to process from right-to-left easily.
          --    The Luhn algorithm doubles every second digit starting from the right.
          reversedDigits = reverse digits

          -- 5. Apply the Luhn transformation:
          --    Iterate through the reversed digits with their indices.
          --    - Digits at odd indices (1, 3, 5, ...) are doubled and adjusted.
          --    - Digits at even indices (0, 2, 4, ...) remain unchanged.
          --    This corresponds to doubling every second digit from the actual rightmost digit.
          transformedDigits = zipWith applyLuhnStep [0..] reversedDigits
            where
              applyLuhnStep index digit
                | odd index = doubleAndAdjust digit
                | otherwise = digit

          -- 6. Sum all the digits after the transformation.
          totalSum = sum transformedDigits
      -- 7. The number is valid if the total sum is divisible by 10.
      in totalSum `mod` 10 == 0
