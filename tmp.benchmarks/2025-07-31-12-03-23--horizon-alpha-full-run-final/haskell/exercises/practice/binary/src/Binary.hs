module Binary (toDecimal) where

-- | Convert a binary string to its decimal representation.
-- Returns 0 for invalid inputs (any character other than '0' or '1').
toDecimal :: String -> Int
toDecimal xs
  | all isBit xs = foldl step 0 xs
  | otherwise    = 0
  where
    isBit '0' = True
    isBit '1' = True
    isBit _   = False

    step acc c =
      let bit = if c == '1' then 1 else 0
       in acc * 2 + bit
