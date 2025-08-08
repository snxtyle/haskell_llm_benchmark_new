module Atbash (decode, encode) where

import Data.Char (isAlphaNum, isLetter, toLower, ord, chr)
import Data.List (intercalate)

decode :: String -> String
decode = transformLetters . sanitize

encode :: String -> String
encode = addGrouping . transformLetters . sanitize

-- Helpers

sanitize :: String -> String
sanitize = map toLower . filter isAlphaNum

transformLetters :: String -> String
transformLetters = map (\c -> if isLetter c then atbash c else c)

atbash :: Char -> Char
atbash c
  | 'a' <= c && c <= 'z' = chr (ord 'a' + ord 'z' - ord c)
  | otherwise            = c

addGrouping :: String -> String
addGrouping = intercalate " " . chunksOf 5

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0    = [xs]
  | otherwise = go xs
  where
    go [] = []
    go ys = let (h, t) = splitAt n ys in h : go t
