module Diamond (diamond) where

import Data.Char (ord, chr, isUpper)

diamond :: Char -> Maybe [String]
diamond c
  | not (isUpper c) || c < 'A' || c > 'Z' = Nothing
  | otherwise = Just (createDiamond c)

createDiamond :: Char -> [String]
createDiamond c = topHalf ++ bottomHalf
  where
    n = ord c - ord 'A'
    topHalf = [createRow i n | i <- [0..n]]
    bottomHalf = [createRow i n | i <- [n-1, n-2..0]]

createRow :: Int -> Int -> String
createRow i n
  | i == 0 = replicate n ' ' ++ "A" ++ replicate n ' '
  | otherwise = replicate (n - i) ' ' ++ [letter] ++ replicate (2 * i - 1) ' ' ++ [letter] ++ replicate (n - i) ' '
  where
    letter = chr (ord 'A' + i)
