module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- The size of the alphabet
m :: Int
m = 26

-- Check if two numbers are coprime
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- Modular multiplicative inverse of a mod m
modInv :: Int -> Int -> Maybe Int
modInv a m = let a' = a `mod` m
                 inv = [x | x <- [1..m-1], (a' * x) `mod` m == 1]
             in case inv of
                  (x:_) -> Just x
                  []    -> Nothing

-- Convert a character to its alphabet index (0-based, a-z)
charToIndex :: Char -> Int
charToIndex c = ord (toLower c) - ord 'a'

-- Convert an index (0-based) to a character
indexToChar :: Int -> Char
indexToChar i = chr (i + ord 'a')

-- Clean input: keep only letters and digits, convert to lowercase
cleanInput :: String -> String
cleanInput = map toLower . filter (\c -> isAlpha c || isDigit c)

-- Group a string into chunks of n characters
groupN :: Int -> String -> [String]
groupN _ [] = []
groupN n xs = let (g, rest) = splitAt n xs in g : groupN n rest

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (coprime a m) = Nothing
  | otherwise =
      let cleaned = cleanInput plainText
          encoded = map encodeChar cleaned
          grouped = intercalate " " (groupN 5 encoded)
      in Just grouped
  where
    encodeChar c
      | isAlpha c = indexToChar $ (a * charToIndex c + b) `mod` m
      | isDigit c = c
      | otherwise = ' ' -- Should not happen due to cleanInput

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (coprime a m) = Nothing
  | otherwise =
      case modInv a m of
        Nothing -> Nothing
        Just aInv ->
          let cleaned = map toLower $ filter (\c -> isAlpha c || isDigit c) cipherText
              decoded = map (decodeChar aInv) cleaned
          in Just decoded
  where
    decodeChar aInv c
      | isAlpha c =
          let y = charToIndex c
              x = (aInv * (y - b)) `mod` m
          in indexToChar (if x < 0 then x + m else x)
      | isDigit c = c
      | otherwise = ' ' -- Should not happen due to filter
