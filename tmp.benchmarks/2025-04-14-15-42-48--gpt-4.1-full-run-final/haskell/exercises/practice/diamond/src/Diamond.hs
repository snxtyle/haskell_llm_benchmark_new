module Diamond (diamond) where

import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond c
  | not (isValid c) = Nothing
  | otherwise       = Just $ map (row n) [0..(2 * n - 2)]
  where
    isValid ch = ch >= 'A' && ch <= 'Z'
    n = ord c - ord 'A' + 1

    row :: Int -> Int -> String
    row size i =
      let
        -- Determine which letter for this row
        letterIdx = if i < size then i else 2 * size - 2 - i
        letter = chr (ord 'A' + letterIdx)
        -- Number of leading/trailing spaces
        outerSpaces = abs (size - 1 - i)
        -- Number of spaces between the letters (for rows with two letters)
        innerSpaces = if letterIdx == 0 then 0 else 2 * letterIdx - 1
      in if letterIdx == 0
         then replicate outerSpaces ' ' ++ [letter] ++ replicate outerSpaces ' '
         else replicate outerSpaces ' ' ++ [letter] ++ replicate innerSpaces ' ' ++ [letter] ++ replicate outerSpaces ' '
