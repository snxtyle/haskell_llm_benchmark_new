module Series (slices) where

import Data.Char (digitToInt)

-- | Return all contiguous substrings of length @n@ as lists of digits.
--   The substrings are returned in the order they appear in the input.
--
--   Error cases follow the conventions used by the canonical tests:
--
--   * An empty input string is invalid.
--   * A slice length of 0 is invalid.
--   * A negative slice length is invalid.
--   * A slice length greater than the input length is invalid.
slices :: Int -> String -> [[Int]]
slices n xs
  | null xs          = error "series cannot be empty"
  | n < 0            = error "slice length cannot be negative"
  | n == 0           = error "slice length cannot be zero"
  | n > length xs    = error "slice length cannot be greater than series length"
  | otherwise        =
      let digits = map digitToInt xs
          len    = length xs
      in [ take n (drop i digits) | i <- [0 .. len - n] ]
