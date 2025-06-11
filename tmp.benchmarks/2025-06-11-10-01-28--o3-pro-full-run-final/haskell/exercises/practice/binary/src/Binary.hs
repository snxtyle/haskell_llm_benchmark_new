module Binary (toDecimal) where

import Data.List (foldl')

-- | Convert a binary number (given as a String consisting of '0' and '1')
--   to its decimal representation (as an Int).
--   If the input contains any characters other than '0' or '1',
--   the function returns 0, treating the whole input as invalid.
toDecimal :: String -> Int
toDecimal xs
  | all isBinaryDigit xs = foldl' step 0 xs
  | otherwise            = 0
  where
    isBinaryDigit c = c == '0' || c == '1'

    -- Accumulate the decimal value as we traverse the string
    step :: Int -> Char -> Int
    step acc '0' = acc * 2
    step acc '1' = acc * 2 + 1
    step acc _   = acc  -- This case is unreachable due to the guard above
