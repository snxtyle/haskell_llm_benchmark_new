module Binary (toDecimal) where

import Data.Char (digitToInt)
import Data.List (foldl')

-- | Convert a binary string to its decimal value.
--   If the input string is empty or contains any character
--   other than '0' or '1', the function returns 0.
toDecimal :: String -> Int
toDecimal xs
  | null xs         = 0
  | all isBinary xs = foldl' step 0 xs
  | otherwise       = 0
  where
    isBinary c = c == '0' || c == '1'
    step acc c = acc * 2 + digitToInt c
