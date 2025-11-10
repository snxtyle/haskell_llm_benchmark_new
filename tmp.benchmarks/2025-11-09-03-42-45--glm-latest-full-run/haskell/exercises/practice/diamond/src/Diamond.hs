module Diamond (diamond) where

import Data.Char (ord, chr)

-- | Generates a single line of the diamond.
--   Takes the maximum index (n) and the current index (i).
makeLine :: Int -> Int -> String
makeLine n i =
  let outerSpaces = n - i
      letter = chr (ord 'A' + i)
      innerSpaces = 2 * i - 1
  in if i == 0
     then replicate outerSpaces ' ' ++ [letter] ++ replicate outerSpaces ' '
     else replicate outerSpaces ' ' ++ [letter] ++ replicate innerSpaces ' ' ++ [letter] ++ replicate outerSpaces ' '

-- | Generates a diamond shape for a given letter.
--   Returns Nothing if the character is not an uppercase letter A-Z.
diamond :: Char -> Maybe [String]
diamond c
  | not ('A' <= c && c <= 'Z') = Nothing
  | otherwise =
      let n = ord c - ord 'A' -- The index of the input letter, from 0 for 'A' to 25 for 'Z'
          -- Create the sequence of indices for the lines: 0, 1, ..., n, n-1, ..., 1, 0
          indices = [0..n] ++ reverse [0..n-1]
      in Just (map (makeLine n) indices)
