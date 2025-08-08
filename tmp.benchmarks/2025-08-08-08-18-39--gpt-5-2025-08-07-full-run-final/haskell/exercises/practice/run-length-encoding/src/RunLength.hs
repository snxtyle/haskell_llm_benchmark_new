module RunLength (decode, encode) where

import Data.Char (isDigit, digitToInt)
import Data.List (group)

decode :: String -> String
decode = go 0
  where
    go :: Int -> String -> String
    go n [] = []
    go n (c:cs)
      | isDigit c = go (n * 10 + digitToInt c) cs
      | otherwise =
          let count = if n == 0 then 1 else n
          in replicate count c ++ go 0 cs

encode :: String -> String
encode =
  concatMap encodeGroup . group
  where
    encodeGroup :: String -> String
    encodeGroup [] = []
    encodeGroup g =
      let n = length g
          c = head g
      in if n == 1 then [c] else show n ++ [c]
