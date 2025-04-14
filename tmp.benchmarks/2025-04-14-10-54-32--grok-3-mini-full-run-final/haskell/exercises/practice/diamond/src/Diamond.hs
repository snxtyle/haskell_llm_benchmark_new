module Diamond (diamond) where

import Data.Char (ord, chr)
import Data.Maybe (fromMaybe)

diamond :: Char -> Maybe [String]
diamond c
  | c >= 'A' && c <= 'Z' = Just (generateDiamond c)
  | otherwise = Nothing

generateDiamond :: Char -> [String]
generateDiamond c = 
  let n = ord c - ord 'A' + 1  -- Position of c (A=1, B=2, etc.)
      totalWidth = 2 * n - 1   -- Total width of the diamond
      topHalf = [buildRow i n totalWidth | i <- [0..n-1]]  -- Rows 0 to n-1
      bottomHalf = tail (reverse topHalf)  -- Mirror top half, excluding the middle row
  in topHalf ++ bottomHalf  -- Full diamond

buildRow :: Int -> Int -> Int -> String
buildRow i n totalWidth =
  let letter = chr (ord 'A' + i)  -- Current letter for this row
      leadingSpaces = replicate (n - 1 - i) ' '  -- Leading spaces
      middleSpaces = if i > 0 then replicate (2 * i - 1) ' ' else ""  -- Spaces between letters
      rowContent = leadingSpaces ++ [letter] ++ middleSpaces ++ (if i > 0 then [letter] else "")
      trailingSpaces = replicate (totalWidth - length rowContent) ' '  -- Add trailing spaces to reach total width
  in rowContent ++ trailingSpaces
