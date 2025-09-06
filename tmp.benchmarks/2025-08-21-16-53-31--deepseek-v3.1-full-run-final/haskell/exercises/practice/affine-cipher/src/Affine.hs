module Affine (decode, encode) where

import Data.Char (isLetter, isDigit, toLower)
import Data.Maybe (fromJust)

-- Check if two numbers are coprime
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- Find modular multiplicative inverse
modInverse :: Int -> Int -> Maybe Int
modInverse a m
  | coprime a m = Just $ head [x | x <- [1..m], (a * x) `mod` m == 1]
  | otherwise = Nothing

-- Convert char to number (a=0, b=1, ..., z=25)
charToNum :: Char -> Int
charToNum c = fromEnum (toLower c) - fromEnum 'a'

-- Convert number to char (0=a, 1=b, ..., 25=z)
numToChar :: Int -> Char
numToChar n = toEnum (n + fromEnum 'a')

-- Encrypt/decrypt a single character
transformChar :: (Int, Int) -> (Int -> Int) -> Char -> Char
transformChar (a, b) f c
  | isLetter c = numToChar $ f (charToNum c) `mod` 26
  | isDigit c = c
  | otherwise = error "Should not reach here - invalid character"

-- Process text with affine cipher
processText :: (Int, Int) -> (Int -> Int) -> String -> String
processText key f text = map (transformChar key f) $ filter (\c -> isLetter c || isDigit c) text

-- Group text into chunks of 5 characters
groupText :: String -> String
groupText [] = []
groupText s
  | length s <= 5 = s
  | otherwise = take 5 s ++ " " ++ groupText (drop 5 s)

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (coprime a 26) = Nothing
  | otherwise = Just $ groupText $ processText (a, b) (\x -> (a * x + b) `mod` 26) plainText

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText =
  case modInverse a 26 of
    Nothing -> Nothing
    Just inverse -> Just $ processText (a, b) (\y -> (inverse * (y - b)) `mod` 26) cipherText
