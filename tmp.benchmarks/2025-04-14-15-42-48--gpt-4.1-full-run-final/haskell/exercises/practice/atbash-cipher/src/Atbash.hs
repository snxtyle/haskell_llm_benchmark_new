module Atbash (decode, encode) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLetter, isSpace, toLower, ord, chr)

-- Helper: Atbash substitution for a single character
atbashChar :: Char -> Char
atbashChar c
  | isAlpha c = chr (ord 'a' + (25 - (ord (toLower c) - ord 'a')))
  | otherwise = c

-- Helper: Remove all non-alphanumeric characters, convert to lowercase
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Helper: Group a string into chunks of n, separated by spaces
groupN :: Int -> String -> String
groupN n = unwords . go
  where
    go [] = []
    go xs = let (h, t) = splitAt n xs in h : go t

encode :: String -> String
encode plainText =
  let norm = normalize plainText
      encoded = map (\c -> if isAlpha c then atbashChar c else c) norm
      -- Keep digits as is, only encode letters, group in 5s
      grouped = groupN 5 encoded
  in grouped

decode :: String -> String
decode cipherText =
  let norm = filter isAlphaNum cipherText
      decoded = map (\c -> if isAlpha c then atbashChar c else c) (map toLower norm)
  in decoded
