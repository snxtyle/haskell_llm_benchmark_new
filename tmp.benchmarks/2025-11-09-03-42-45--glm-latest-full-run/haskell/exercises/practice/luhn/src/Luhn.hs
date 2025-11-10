module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

-- | Checks if a string is a valid number according to the Luhn algorithm.
isValid :: String -> Bool
isValid n =
  let cleaned = filter (/= ' ') n
      allDigits = all isDigit cleaned
      lengthOk = length cleaned > 1
  in if not (allDigits && lengthOk)
     then False
     else
       let digits = map digitToInt cleaned
           -- Process from the right, so reverse the list.
           reversedDigits = reverse digits
           -- Function to double a digit and subtract 9 if the result is > 9.
           transform d = let doubled = d * 2 in if doubled > 9 then doubled - 9 else doubled
           -- Apply the transformation to every second digit.
           -- We cycle between the identity function (do nothing) and the transform function.
           transformedDigits = zipWith ($) (cycle [id, transform]) reversedDigits
           -- The number is valid if the sum of the digits is a multiple of 10.
           total = sum transformedDigits
       in total `mod` 10 == 0
