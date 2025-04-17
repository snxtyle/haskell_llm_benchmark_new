module ArmstrongNumbers (armstrong) where

-- | Determine whether a number is an Armstrong number.
--   An Armstrong number is equal to the sum of its own digits each raised
--   to the power of the number of digits.
--
--   Examples:
--     9   -> True  (9 == 9^1)
--     10  -> False (10 /= 1^2 + 0^2)
--     153 -> True  (153 == 1^3 + 5^3 + 3^3)
armstrong :: Integral a => a -> Bool
armstrong n = tn == sum (map (^ k) ds)
  where
    tn = toInteger n                 -- Original number as an Integer
    ds = digits (abs tn)             -- Digits of the absolute value
    k  = fromIntegral (length ds)    -- Number of digits (exponent)

    -- | Convert a non‑negative integer to its list of base‑10 digits.
    digits :: Integer -> [Integer]
    digits 0 = [0]
    digits x = go x
      where
        go 0 = []
        go y =
          let (q, r) = y `divMod` 10
          in r : go q
