module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (unfoldr)

-- Alphabet size
m :: Int
m = 26

-- Convert a letter to its numeric value (0-25)
letterToNum :: Char -> Int
letterToNum c = ord (toLower c) - ord 'a'

-- Convert a numeric value (0-25) to a letter
numToLetter :: Int -> Char
numToLetter n = chr (n + ord 'a')

-- Calculate the greatest common divisor
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Check if two numbers are coprime
areCoprime :: Int -> Int -> Bool
areCoprime a b = gcd' a b == 1

-- Extended Euclidean algorithm to find modular multiplicative inverse
extendedGcd :: Int -> Int -> (Int, Int, Int)
extendedGcd a 0 = (a, 1, 0)
extendedGcd a b = (g, y, x - (a `div` b) * y)
  where
    (g, x, y) = extendedGcd b (a `mod` b)

-- Find modular multiplicative inverse of a mod m
modInverse :: Int -> Int -> Maybe Int
modInverse a m'
  | g /= 1 = Nothing
  | otherwise = Just ((x `mod` m' + m') `mod` m')
  where
    (g, x, _) = extendedGcd a m'

-- Encode a single character
encodeChar :: Int -> Int -> Char -> Char
encodeChar a b c
  | isAlpha c = numToLetter (((a * letterToNum c) + b) `mod` m)
  | otherwise = c

-- Decode a single character
decodeChar :: Int -> Int -> Char -> Char
decodeChar aInv b c
  | isAlpha c = numToLetter (((aInv * (letterToNum c - b)) `mod` m + m) `mod` m)
  | otherwise = c

-- Group text into chunks of n characters
groupsOf :: Int -> String -> [String]
groupsOf n = unfoldr (\s -> if null s then Nothing else Just (splitAt n s))

-- Process text for encoding/decoding (filter and convert to lowercase)
processText :: String -> String
processText = filter (\c -> isAlpha c || isDigit c)

-- Format output in groups of 5
formatOutput :: String -> String
formatOutput = unwords . groupsOf 5

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (areCoprime a m) = Nothing
  | otherwise = Just $ formatOutput $ map (encodeChar a b) $ processText plainText

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (areCoprime a m) = Nothing
  | otherwise = case modInverse a m of
      Nothing -> Nothing
      Just aInv -> Just $ map (decodeChar aInv b) $ processText cipherText
