module Binary (toDecimal) where

import Data.Char (digitToInt)

-- | Convert a binary string to its decimal representation.
--   Returns 0 for any invalid input (characters other than '0' or '1').
toDecimal :: String -> Int
toDecimal xs
  | all (`elem` "01") xs = foldl step 0 xs
  | otherwise            = 0
  where
    step acc c = acc * 2 + digitToInt c
