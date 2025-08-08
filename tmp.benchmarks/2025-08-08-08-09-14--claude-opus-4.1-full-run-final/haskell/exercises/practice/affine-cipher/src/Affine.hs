module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (unfoldr)

-- Check if two numbers are coprime (GCD = 1)
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- Find modular multiplicative inverse using extended Euclidean algorithm
modInverse :: Int -> Int -> Maybe Int
modInverse a m
  | coprime a m = Just $ (x `mod` m + m) `mod` m
  | otherwise = Nothing
  where
    (_, x, _) = extGCD a m
    extGCD :: Int -> Int -> (Int, Int, Int)
    extGCD 0 b = (b, 0, 1)
    extGCD a' b' = (g, y - (b' `div` a') * x', x')
      where
        (g, x', y) = extGCD (b' `mod` a') a'

-- Convert a character to its position (a=0, b=1, ..., z=25)
charToPos :: Char -> Int
charToPos c = ord (toLower c) - ord 'a'

-- Convert a position back to a character
posToChar :: Int -> Char
posToChar n = chr (n + ord 'a')

-- Encode a single character
encodeChar :: Int -> Int -> Char -> Char
encodeChar a b c
  | isAlpha c = posToChar $ ((a * charToPos c + b) `mod` 26)
  | isDigit c = c
  | otherwise = c

-- Decode a single character
decodeChar :: Int -> Int -> Char -> Char
decodeChar aInv b c
  | isAlpha c = posToChar $ ((aInv * (charToPos c - b)) `mod` 26 + 26) `mod` 26
  | isDigit c = c
  | otherwise = c

-- Group characters into chunks of specified size
groupBy :: Int -> String -> [String]
groupBy n = unfoldr (\s -> if null s then Nothing else Just (splitAt n s))

-- Clean and process text for encoding
cleanForEncode :: String -> String
cleanForEncode = filter (\c -> isAlpha c || isDigit c) . map toLower

-- Clean text for decoding (remove spaces)
cleanForDecode :: String -> String
cleanForDecode = filter (/= ' ')

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (coprime a 26) = Nothing
  | otherwise = Just $ unwords $ groupBy 5 encoded
  where
    cleaned = cleanForEncode plainText
    encoded = map (encodeChar a b) cleaned

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText = do
  aInv <- modInverse a 26
  let cleaned = cleanForDecode cipherText
  let decoded = map (decodeChar aInv b) cleaned
  return decoded
