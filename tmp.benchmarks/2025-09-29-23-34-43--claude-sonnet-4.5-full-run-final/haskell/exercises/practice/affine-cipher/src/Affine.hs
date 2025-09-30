module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (intercalate)

-- Check if two numbers are coprime (gcd = 1)
coprime :: Int -> Int -> Bool
coprime a m = gcd a m == 1

-- Find modular multiplicative inverse of a mod m
-- Returns Nothing if inverse doesn't exist
modInverse :: Int -> Int -> Maybe Int
modInverse a m
  | not (coprime a m) = Nothing
  | otherwise = Just $ head [x | x <- [1..m-1], (a * x) `mod` m == 1]

-- Encode a single character
encodeChar :: (Int, Int) -> Char -> Char
encodeChar (a, b) c
  | isAlpha c = 
      let i = ord (toLower c) - ord 'a'
          encrypted = (a * i + b) `mod` 26
      in chr (encrypted + ord 'a')
  | otherwise = c

-- Decode a single character
decodeChar :: (Int, Int) -> Int -> Char -> Char
decodeChar (a, b) aInv c
  | isAlpha c =
      let y = ord (toLower c) - ord 'a'
          decrypted = (aInv * (y - b)) `mod` 26
      in chr (decrypted + ord 'a')
  | otherwise = c

-- Split string into chunks of n characters
chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

encode :: (Int, Int) -> String -> Maybe String
encode key@(a, b) plainText
  | not (coprime a 26) = Nothing
  | otherwise = 
      let filtered = [c | c <- plainText, isAlpha c || isDigit c]
          encoded = map (encodeChar key) filtered
          grouped = intercalate " " (chunksOf 5 encoded)
      in Just grouped

decode :: (Int, Int) -> String -> Maybe String
decode key@(a, b) cipherText = do
  aInv <- modInverse a 26
  let filtered = [c | c <- cipherText, isAlpha c || isDigit c]
      decoded = map (decodeChar key aInv) filtered
  return decoded
