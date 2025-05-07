module ArmstrongNumbers (armstrong) where

-- | Determine whether a number is an Armstrong (narcissistic) number.
--   An Armstrong number is a number that is equal to the sum of its own
--   digits each raised to the power of the number of digits.
--
--   Examples:
--     9   -> True  (9 = 9¹)
--     10  -> False (10 ≠ 1² + 0²)
--     153 -> True  (153 = 1³ + 5³ + 3³)
--
--   The function works for any Integral type.  Negative numbers are
--   considered /not/ Armstrong numbers.
armstrong :: Integral a => a -> Bool
armstrong n
  | n < 0     = False
  | otherwise = fromIntegral n == sum (map (^ numDigits) digitsList)
  where
    digitsList :: [Integer]
    digitsList = toDigits (fromIntegral n)

    numDigits :: Int
    numDigits = length digitsList

    -- Convert a non‑negative integer to its list of decimal digits.
    -- The order of digits is irrelevant for our purposes (sum of powers),
    -- so we collect them least‑significant first, which is convenient.
    toDigits :: Integer -> [Integer]
    toDigits 0 = [0]
    toDigits x = go x
      where
        go 0 = []
        go y = let (q, r) = y `quotRem` 10 in r : go q
