module Diamond (diamond) where

import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just $ top ++ bottom
  where
    n = ord c - ord 'A'
    size = 2 * n + 1

    -- Create a single line of the diamond for a given index i (0-based)
    line i
      | i == 0 = replicate n ' ' ++ "A" ++ replicate n ' '
      | otherwise =
          let outerSpaces = n - i
              innerSpaces = 2 * i - 1
              ch = chr (ord 'A' + i)
          in replicate outerSpaces ' ' ++ [ch] ++ replicate innerSpaces ' ' ++ [ch] ++ replicate outerSpaces ' '

    top = [line i | i <- [0..n]]
    bottom = [line i | i <- [n-1,n-2..0]]
