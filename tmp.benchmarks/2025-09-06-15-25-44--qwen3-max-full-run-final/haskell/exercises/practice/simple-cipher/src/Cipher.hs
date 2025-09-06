module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (ord, chr, isLetter, toLower)

-- Encode a string using a key (substitution cipher)
caesarEncode :: String -> String -> String
caesarEncode key text = map encodeChar (zip (map toLower text) (cycle (map toLower key)))
  where
    encodeChar (c, k)
      | isLetter c = chr $ ((ord c - ord 'a' + ord k - ord 'a') `mod` 26) + ord 'a'
      | otherwise = c

-- Decode a string using a key (substitution cipher)
caesarDecode :: String -> String -> String
caesarDecode key encodedText = map decodeChar (zip (map toLower encodedText) (cycle (map toLower key)))
  where
    decodeChar (c, k)
      | isLetter c = chr $ ((ord c - ord 'a' - (ord k - ord 'a')) `mod` 26 + 26) `mod` 26 + ord 'a'
      | otherwise = c

-- Generate a random key and encode the text
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    key <- generateRandomKey 100
    let encoded = caesarEncode key text
    return (encoded, key)

-- Generate a random key of specified length with lowercase letters
generateRandomKey :: Int -> IO String
generateRandomKey len = sequence $ replicate len $ randomRIO ('a', 'z')
