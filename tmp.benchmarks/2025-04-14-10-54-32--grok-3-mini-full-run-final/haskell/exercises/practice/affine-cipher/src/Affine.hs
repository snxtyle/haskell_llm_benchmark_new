module Affine (decode, encode) where

import Data.Char (toLower, isLower, chr, isDigit)
import Data.List (find)

-- Helper function to check if two numbers are coprime
isCoprime :: Int -> Int -> Bool
isCoprime x y = gcd x y == 1

-- Helper function to find the modular multiplicative inverse
modularInverse :: Int -> Int -> Maybe Int
modularInverse a m = find (\x -> (a * x) `mod` m == 1) [1..m-1]

-- Helper function to convert a lowercase letter to its numeric equivalent (0-25)
letterToNum :: Char -> Maybe Int
letterToNum c
  | c >= 'a' && c <= 'z' = Just (fromEnum c - fromEnum 'a')
  | otherwise = Nothing

-- Helper function to convert a number (0-25) back to a lowercase letter
numToLetter :: Int -> Char
numToLetter n = chr (n + fromEnum 'a')  -- Assumes n is in 0..25

-- Main encode function
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (isCoprime a 26) = Nothing  -- a and 26 must be coprime
  | otherwise = Just $ doEncode (a, b) plainText

-- Implementation for encode: Process the string and group output
doEncode :: (Int, Int) -> String -> String
doEncode (a, b) text =
  let lowered = map toLower text  -- Convert to lowercase
      processed = [ if isLower c then encryptLetter c else if isDigit c then c else ' ' | c <- lowered ]  -- Keep digits, encrypt letters, exclude others
      filteredProcessed = filter (/= ' ') processed  -- Remove excluded characters
  in unwords $ chunksOf 5 filteredProcessed  -- Group into chunks of 5 and separate by spaces
  where
    encryptLetter c = case letterToNum c of
      Just i -> numToLetter ((a * i + b) `mod` 26)  -- E(x) = (a * i + b) mod 26
      Nothing -> c  -- Should not happen

-- Helper function to split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Main decode function
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (isCoprime a 26) = Nothing  -- a and 26 must be coprime
  | otherwise = case modularInverse a 26 of
      Nothing -> Nothing  -- Should not happen if isCoprime passed
      Just invA -> Just $ doDecode (invA, b) cipherText

-- Implementation for decode: Process the string by removing spaces and decrypting
doDecode :: (Int, Int) -> String -> String
doDecode (invA, b) text =
  let noSpaces = filter (/= ' ') text  -- Remove spaces
      lowered = map toLower noSpaces  -- Convert to lowercase
      lettersOnly = filter isLower lowered  -- Filter to only lowercase letters
  in map decryptLetter lettersOnly  -- Decrypt each letter
  where
    decryptLetter c = case letterToNum c of
      Just i -> numToLetter ((invA * ((i - b + 26) `mod` 26)) `mod` 26)  -- D(y) = invA * (y - b) mod 26, adjusted for negative
      Nothing -> c  -- Should not happen since we filtered
