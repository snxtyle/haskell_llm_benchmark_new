module Affine (decode, encode) where

import Data.Char (chr, isAlpha, isDigit, isLetter, isLower, isUpper, ord, toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- Length of the alphabet
m :: Int
m = 26

-- Check if two numbers are coprime (gcd == 1)
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- Extended Euclidean algorithm to find modular inverse
-- Returns Just inverse if exists, Nothing otherwise
modInv :: Int -> Int -> Maybe Int
modInv a m = let (g, x, _) = extendedGCD a m
             in if g == 1 then Just (x `mod` m) else Nothing

-- Extended Euclidean algorithm
extendedGCD :: Int -> Int -> (Int, Int, Int)
extendedGCD 0 b = (b, 0, 1)
extendedGCD a b =
  let (g, s, t) = extendedGCD (b `mod` a) a
  in (g, t - (b `div` a) * s, s)

-- Convert a letter to its 0-based index (a=0, b=1, ..., z=25)
charToIndex :: Char -> Int
charToIndex c = ord (toLower c) - ord 'a'

-- Convert 0-based index to lowercase letter
indexToChar :: Int -> Char
indexToChar i = chr (i + ord 'a')

-- Clean input: remove spaces and punctuation, keep letters and digits
-- Digits are kept but not encrypted/decrypted
cleanInput :: String -> String
cleanInput = filter (\c -> isAlpha c || isDigit c)

-- Group string into chunks of n characters separated by spaces
groupByN :: Int -> String -> String
groupByN n s = intercalate " " (chunk n s)
  where
    chunk _ [] = []
    chunk k xs = let (h, t) = splitAt k xs in h : chunk k t

-- Encode a single character with affine cipher
-- Digits are not encoded, just returned as is
encodeChar :: (Int, Int) -> Char -> Char
encodeChar (a, b) c
  | isAlpha c =
      let i = charToIndex c
          e = (a * i + b) `mod` m
      in indexToChar e
  | otherwise = c

-- Decode a single character with affine cipher
-- Digits are not decoded, just returned as is
decodeChar :: Int -> (Int, Int) -> Char -> Char
decodeChar aInv (a, b) c
  | isAlpha c =
      let y = charToIndex c
          d = (aInv * (y - b)) `mod` m
      in indexToChar d
  | otherwise = c

-- Encode function
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (coprime a m) = Nothing
  | otherwise =
      let cleaned = cleanInput plainText
          encoded = map (encodeChar (a, b)) cleaned
          grouped = groupByN 5 encoded
      in Just grouped

-- Decode function
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (coprime a m) = Nothing
  | otherwise = do
      aInv <- modInv a m
      let cleaned = cleanInput cipherText
          decoded = map (decodeChar aInv (a, b)) cleaned
      return decoded
