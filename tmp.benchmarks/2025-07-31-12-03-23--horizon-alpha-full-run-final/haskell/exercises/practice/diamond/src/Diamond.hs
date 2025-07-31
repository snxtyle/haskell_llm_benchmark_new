module Diamond (diamond) where

import Data.Char (ord)

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just (top ++ tail (reverse top))
  where
    n = ord c - ord 'A'
    width = 2 * n + 1
    -- Build the top half including the middle row for each letter from 'A'..c
    top = [row i | i <- [0 .. n]]
    row i =
      let ch = toEnum (ord 'A' + i) :: Char
          lead = n - i
      in if i == 0
           then replicate lead ' ' ++ [ch] ++ replicate lead ' '
           else
             let inner = width - 2 * lead - 2
             in replicate lead ' ' ++ [ch] ++ replicate inner ' ' ++ [ch] ++ replicate lead ' '
