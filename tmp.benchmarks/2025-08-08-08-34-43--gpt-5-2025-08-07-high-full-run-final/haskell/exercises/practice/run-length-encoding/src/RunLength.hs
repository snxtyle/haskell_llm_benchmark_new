module RunLength (decode, encode) where

import Data.Char (isDigit, digitToInt)
import Data.List (group)

decode :: String -> String
decode encodedText = go 0 encodedText
  where
    go :: Int -> String -> String
    go _ [] = []
    go n (c:cs)
      | isDigit c = go (n * 10 + digitToInt c) cs
      | otherwise = replicate (if n == 0 then 1 else n) c ++ go 0 cs

encode :: String -> String
encode text = concatMap encodeGroup (group text)
  where
    encodeGroup :: String -> String
    encodeGroup [] = []
    encodeGroup xs@(x:_)
      | len == 1  = [x]
      | otherwise = show len ++ [x]
      where
        len = length xs
