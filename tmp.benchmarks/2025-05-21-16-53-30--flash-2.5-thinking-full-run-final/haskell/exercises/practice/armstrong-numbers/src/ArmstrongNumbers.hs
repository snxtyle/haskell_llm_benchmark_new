module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

-- | Determines if a number is an Armstrong number.
-- An Armstrong number is a number that is the sum of its own digits
-- each raised to the power of the number of digits.
--
-- Examples:
--   - 9 is an Armstrong number (9 = 9^1)
--   - 153 is an Armstrong number (153 = 1^3 + 5^3 + 3^3)
--   - 10 is not an Armstrong number (10 != 1^2 + 0^2)
armstrong :: (Integral a, Show a) => a -> Bool
armstrong n
  | n < 0 = False -- Armstrong numbers are typically defined for non-negative integers.
  | otherwise =
      let
        -- Convert the number to its string representation to easily get digits and count them.
        s = show n
        -- The number of digits determines the power to which each digit is raised.
        numDigits = length s
        -- Calculate the sum of each digit raised to the power of numDigits.
        -- digitToInt converts Char to Int.
        -- fromIntegral converts Int to the generic Integral type 'a'.
        -- (^) performs exponentiation.
        sumOfPowers = sum $ map (\c -> (fromIntegral (digitToInt c)) ^ numDigits) s
      in
        -- An Armstrong number is equal to this sum.
        sumOfPowers == n
