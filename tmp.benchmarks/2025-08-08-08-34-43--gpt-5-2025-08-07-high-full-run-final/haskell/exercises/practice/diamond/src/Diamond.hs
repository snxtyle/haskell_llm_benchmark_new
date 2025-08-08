module Diamond (diamond) where

import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise          = Just (top ++ bottom)
  where
    k = ord c - ord 'A'
    top = [makeRow k i | i <- [0 .. k]]
    bottom = [makeRow k i | i <- [k - 1, k - 2 .. 0]]

makeRow :: Int -> Int -> String
makeRow k i =
  let pad = replicate (k - i) ' '
      ch  = chr (ord 'A' + i)
  in if i == 0
       then pad ++ [ch] ++ pad
       else
         let inner = replicate (2 * i - 1) ' '
         in pad ++ [ch] ++ inner ++ [ch] ++ pad
