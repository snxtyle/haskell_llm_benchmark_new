module ArmstrongNumbers (armstrong) where

-- | Determine whether a given non-negative integer is an Armstrong number.
--   An Armstrong (or narcissistic) number is equal to the sum of its own
--   digits each raised to the power of the number of digits.
--
--   153 = 1^3 + 5^3 + 3^3  →  Armstrong
--   154 = 1^3 + 5^3 + 4^3  →  not Armstrong
--
--   Negative inputs are treated as non-Armstrong.
--
--   The implementation works for any Integral type (Int, Integer, …).
armstrong :: Integral a => a -> Bool
armstrong n
  | n < 0     = False
  | otherwise = sum (map (^ len) digits) == n'
  where
    n'     = toInteger n
    digits = toDigits n'
    len    = length digits

-- Convert a non-negative integer to the list of its base-10 digits.
-- The least-significant digit comes first; the order is irrelevant for
-- the Armstrong check because only the set of digits and their count
-- are needed.
toDigits :: Integer -> [Integer]
toDigits 0 = [0]
toDigits x = go x
  where
    go 0 = []
    go y = let (q, r) = y `quotRem` 10
           in r : go q
