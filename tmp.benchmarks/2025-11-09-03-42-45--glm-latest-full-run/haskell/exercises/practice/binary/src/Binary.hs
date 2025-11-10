module Binary (toDecimal) where

import Data.List (foldl')

-- | Convert a binary string to its decimal integer equivalent.
-- Returns 0 if the input string contains any character other than '0' or '1'.
toDecimal :: String -> Int
toDecimal xs
  | not (all (`elem` "01") xs) = 0
  | otherwise = foldl' step 0 xs
  where
    step acc c = acc * 2 + if c == '1' then 1 else 0
