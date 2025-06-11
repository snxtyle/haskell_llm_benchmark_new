module Atbash (decode, encode) where

import Data.Char (isAlpha, isAlphaNum, toLower, ord, chr)
import Data.List (intercalate)

decode :: String -> String
decode = map transpose . filter isAlphaNum

encode :: String -> String
encode = intercalate " " . groupsOf 5 . map transpose . filter isAlphaNum . map toLower

transpose :: Char -> Char
transpose c
  | isAlpha c = chr (ord 'a' + ord 'z' - ord c)
  | otherwise = c

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
