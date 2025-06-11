module Affine (decode, encode) where

import Data.Char (chr, isAlpha, isDigit, ord, toLower)
import Data.List (find, intercalate)

m :: Int
m = 26

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

coprime :: Int -> Int -> Bool
coprime a n = gcd a n == 1

mmi :: Int -> Int -> Maybe Int
mmi a n = find (\x -> (a * x) `mod` n == 1) [1 .. n - 1]

-- Custom implementation of chunksOf to avoid extra dependencies
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (coprime a m) = Nothing
  | otherwise = Just $ intercalate " " $ chunksOf 5 $ map (encryptChar a b) $ filter isAlphaNum $ map toLower plainText

encryptChar :: Int -> Int -> Char -> Char
encryptChar a b c
  | isDigit c = c
  | isAlpha c =
      let i = ord c - ord 'a'
          encodedI = (a * i + b) `mod` m
       in chr (encodedI + ord 'a')
  | otherwise = c -- Should not be reached due to filter

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText =
  case mmi a m of
    Nothing -> Nothing
    Just mmiVal -> Just $ map (decryptChar mmiVal b) $ filter isAlphaNum cipherText

decryptChar :: Int -> Int -> Char -> Char
decryptChar mmiVal b c
  | isDigit c = c
  | isAlpha c =
      let y = ord c - ord 'a'
          decodedI = (mmiVal * (y - b)) `mod` m
       in chr (decodedI + ord 'a')
  | otherwise = c -- Should not be reached due to filter
