module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n xs
  -- Handle n=0 case specifically as required by tests
  | n == 0 = replicate (length xs + 1) []
  -- Handle invalid n values
  | n < 0 || n > length xs = []
  -- Generate slices for valid n
  | otherwise =
      -- Get all suffixes of the input string
      let allTails = tails xs
          -- Keep only the tails that are long enough to form a slice of length n
          -- The number of valid slices is (length xs - n + 1)
          validStartTails = take (length xs - n + 1) allTails
          -- Take the first n characters from each valid starting tail
          stringSlices = map (take n) validStartTails
       -- Convert each character in each string slice to an Int
       in map (map digitToInt) stringSlices
