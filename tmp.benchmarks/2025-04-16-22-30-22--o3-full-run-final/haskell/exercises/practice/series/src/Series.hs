module Series (slices) where

import Data.Char (digitToInt)

-- | Given a slice length @n@ and a string consisting of digit characters,
--   return every contiguous subsequence of length @n@ as a list of lists of
--   'Int'.  Each digit character is converted to its corresponding numeric
--   value.
--
--   The behaviour is as follows:
--
--   * A non‑positive slice size yields an error.
--   * An empty input string yields @[]@ for any positive slice size.
--   * A slice size larger than the (non‑empty) input string yields an error.
--
--   Examples:
--
--   >>> slices 3 "49142"
--   [[4,9,1],[9,1,4],[1,4,2]]
--
--   >>> slices 1 ""
--   []
slices :: Int -> String -> [[Int]]
slices n xs
  | n <= 0            = error "slice length must be positive"
  | null xs           = []                                   -- nothing to slice
  | n > len           = error "slice length exceeds input length"
  | otherwise         = [ map digitToInt (take n (drop i xs))
                        | i <- [0 .. len - n]
                        ]
  where
    len = length xs
