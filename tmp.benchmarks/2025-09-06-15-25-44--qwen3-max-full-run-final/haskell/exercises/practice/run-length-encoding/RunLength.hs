module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode [] = []
decode (x:xs)
  | isDigit x = decodeCount xs [x]
  | otherwise = x : decode xs
  where
    decodeCount :: String -> String -> String
    decodeCount [] countDigits = replicate (read countDigits) ' ' -- This shouldn't happen with valid input
    decodeCount (y:ys) countDigits
      | isDigit y = decodeCount ys (countDigits ++ [y])
      | otherwise = replicate (read countDigits) y ++ decode ys

encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup :: String -> String
    encodeGroup [x] = [x]
    encodeGroup xs = show (length xs) ++ [head xs]
