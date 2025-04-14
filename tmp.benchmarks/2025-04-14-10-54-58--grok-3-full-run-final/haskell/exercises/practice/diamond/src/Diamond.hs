module Diamond (diamond) where

import Data.Char (isUpper, ord, chr)

diamond :: Char -> Maybe [String]
diamond c 
    | not (isUpper c) || c < 'A' || c > 'Z' = Nothing
    | otherwise = Just $ buildDiamond c

buildDiamond :: Char -> [String]
buildDiamond c = topHalf ++ tail (reverse topHalf)
  where
    n = ord c - ord 'A' + 1  -- Size parameter: distance from 'A' plus 1
    topHalf = [buildRow i n | i <- [0..n-1]]

buildRow :: Int -> Int -> String
buildRow row total = leftSpaces ++ leftLetter ++ middleSpaces ++ rightLetter ++ rightSpaces
  where
    letter = chr (ord 'A' + row)
    outerSpaces = total - row - 1
    innerSpaces = if row == 0 then 0 else 2 * row - 1
    leftSpaces = replicate outerSpaces ' '
    rightSpaces = replicate outerSpaces ' '
    middleSpaces = replicate innerSpaces ' '
    leftLetter = if row == 0 then [letter] else [letter, ' ']
    rightLetter = if row == 0 then "" else [' ', letter]
