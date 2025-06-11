module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Control.Monad (replicateM)
import Data.Char (chr, isLower, ord)
import Data.List (mapAccumL)
import System.Random (randomRIO)

-- Helper to convert a lowercase character to an integer (a=0, b=1, ...).
charToInt :: Char -> Int
charToInt c = ord c - ord 'a'

-- Helper to convert an integer back to a lowercase character.
intToChar :: Int -> Char
intToChar n = chr (n + ord 'a')

-- Generic transformation function that applies a shifting function to a text
-- using a key. It preserves non-lowercase characters in the text, and only
-- advances the key for lowercase characters.
-- Assumes the key is non-empty and contains only lowercase letters.
transformText :: (Char -> Char -> Char) -> String -> String -> String
transformText shifter key text = snd $ mapAccumL go (cycle key) text
  where
    go :: String -> Char -> (String, Char)
    go keyStream char
      | isLower char =
        let (k : ks) = keyStream
         in (ks, shifter char k)
      | otherwise = (keyStream, char)

-- Encoding character shifter.
encodeChar :: Char -> Char -> Char
encodeChar textChar keyChar = intToChar $ (charToInt textChar + charToInt keyChar) `mod` 26

-- Encodes a plaintext using a key.
caesarEncode :: String -> String -> String
caesarEncode = transformText encodeChar

-- Decoding character shifter.
decodeChar :: Char -> Char -> Char
decodeChar encodedChar keyChar = intToChar $ (charToInt encodedChar - charToInt keyChar + 26) `mod` 26

-- Decodes a ciphertext using a key.
caesarDecode :: String -> String -> String
caesarDecode = transformText decodeChar

-- Generates a random key and uses it to encode a plaintext.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  -- Generate a random key of 100 lowercase characters.
  key <- replicateM 100 (randomRIO ('a', 'z'))
  let encodedText = caesarEncode key text
  return (key, encodedText)
