module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (ord, chr, isLower)

-- Helper function to shift a single character
shiftChar :: Int -> Char -> Char
shiftChar shift c
  | isLower c = chr $ ((ord c - ord 'a' + shift) `mod` 26) + ord 'a'
  | otherwise = c

-- Helper function to convert key character to shift value
keyToShift :: Char -> Int
keyToShift c = ord c - ord 'a'

-- Decode a Caesar cipher using a key
caesarDecode :: String -> String -> String
caesarDecode key encodedText = zipWith decodeChar (cycle key) encodedText
  where
    decodeChar k c
      | isLower c && isLower k = shiftChar (negate $ keyToShift k) c
      | otherwise = c

-- Encode text using a Caesar cipher with a key
caesarEncode :: String -> String -> String
caesarEncode key text = zipWith encodeChar (cycle key) text
  where
    encodeChar k c
      | isLower c && isLower k = shiftChar (keyToShift k) c
      | otherwise = c

-- Generate a random key and encode the text
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  keyLength <- randomRIO (100, 150) :: IO Int
  key <- sequence $ replicate keyLength randomLowerChar
  let encoded = caesarEncode key text
  return (key, encoded)
  where
    randomLowerChar = do
      n <- randomRIO (0, 25) :: IO Int
      return $ chr (ord 'a' + n)
