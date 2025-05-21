module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0         = []       -- Negative slice length is invalid
  | n == 0        = [[]]     -- Zero-length slice: one empty slice
  | n > length xs = []       -- Slice length greater than the string length
  | otherwise     =
      let
        -- Generate all suffixes of the input string.
        -- Example: tails "abc" -> ["abc", "bc", "c", ""]
        allSuffixes = tails xs
        -- Filter out suffixes that are too short to form a slice of length n.
        -- This leaves only the starting points for valid slices.
        validStartingPoints = filter (\s -> length s >= n) allSuffixes
        -- For each valid starting point, take the first n characters to form the string slice.
        stringSlices = map (take n) validStartingPoints
        -- Convert each character in each string slice to its integer representation.
        -- This results in a list of lists of integers.
        intSlices = map (map digitToInt) stringSlices
      in
        intSlices
