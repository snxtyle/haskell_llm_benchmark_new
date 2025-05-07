module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr)
import System.Random (randomRIO)

-- Ensure the key only contains lowercase a–z
validateKey :: String -> Bool
validateKey = all (\c -> c >= 'a' && c <= 'z')

-- Shift a lowercase letter forward by shift; leave others untouched
shiftChar :: Int -> Char -> Char
shiftChar shift c
  | c >= 'a' && c <= 'z' = chr $ ord 'a' + ((ord c - ord 'a' + shift) `mod` 26)
  | otherwise            = c

-- Shift a lowercase letter backward by shift; leave others untouched
unshiftChar :: Int -> Char -> Char
unshiftChar shift c
  | c >= 'a' && c <= 'z' = chr $ ord 'a' + ((ord c - ord 'a' - shift + 26) `mod` 26)
  | otherwise            = c

-- Encode using a repeating key of lowercase letters
caesarEncode :: String -> String -> String
caesarEncode key text
  | not (validateKey key) = error "Key must contain only lowercase letters"
  | otherwise =
      let shifts = cycle $ map (\c -> ord c - ord 'a') key
      in zipWith shiftChar shifts text

-- Decode by reversing the shifts driven by the key
caesarDecode :: String -> String -> String
caesarDecode key encodedText
  | not (validateKey key) = error "Key must contain only lowercase letters"
  | otherwise =
      let shifts = cycle $ map (\c -> ord c - ord 'a') key
      in zipWith unshiftChar shifts encodedText

-- Generate a random lowercase key (length = max 100 (length text)), then encode
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  let n = max 100 (length text)
  key <- sequence $ replicate n (randomRIO ('a','z'))
  let encoded = caesarEncode key text
  return (encoded, key)
