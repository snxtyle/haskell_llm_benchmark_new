module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

-- Encode a string using run-length encoding.
-- Consecutive runs are replaced with count and character (count omitted if 1).
encode :: String -> String
encode =
  concatMap encodeRun . group
  where
    encodeRun :: String -> String
    encodeRun [] = ""
    encodeRun xs@(x:_) =
      let n = length xs
      in (if n == 1 then "" else show n) ++ [x]

-- Decode a run-length encoded string.
-- Digits represent the count for the following character.
-- If no digits precede a character, the count is 1.
decode :: String -> String
decode = go "" 0
  where
    go :: String -> Int -> String -> String
    go acc _ [] = acc
    go acc n (c:cs)
      | isDigit c =
          let n' = n * 10 + (fromEnum c - fromEnum '0')
          in go acc n' cs
      | otherwise =
          let count = if n == 0 then 1 else n
              expanded = replicate count c
          in go (acc ++ expanded) 0 cs
