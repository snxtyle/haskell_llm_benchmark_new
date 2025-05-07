module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: (Integral a, Show a) => a -> Bool
armstrong n
    | n < 0     = False -- Armstrong numbers are generally defined for non-negative integers.
    | otherwise = n == sumOfPowers
    where
        s :: String
        s = show n

        numDigits :: Int
        numDigits = length s

        -- Convert each character in the string representation of n to an integer digit,
        -- then convert it to type 'a' (the type of n) using fromIntegral.
        -- For fromIntegral :: (Integral b, Num c) => b -> c:
        -- b is Int (from digitToInt)
        -- c is a (so 'a' must be Num, which is implied by Integral a)
        digits :: [a]
        digits = map (fromIntegral . digitToInt) s

        -- Calculate sum of each digit raised to the power of numDigits.
        -- The base of the power (d) is of type 'a'. (Num a required, implied by Integral a)
        -- The exponent (numDigits) is Int. (Integral required for exponent type)
        -- The result of d ^ numDigits is of type 'a'.
        -- The sum is also of type 'a'. (Num a required for elements of sum, implied by Integral a)
        sumOfPowers :: a
        sumOfPowers = sum [d ^ numDigits | d <- digits]
