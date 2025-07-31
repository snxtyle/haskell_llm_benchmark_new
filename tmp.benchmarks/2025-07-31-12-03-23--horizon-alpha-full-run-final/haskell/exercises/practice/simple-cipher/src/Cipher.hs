module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (isLower, ord, chr)

-- Shift a single lowercase character by n positions (can be negative).
shiftChar :: Int -> Char -> Char
shiftChar n c
  | isLower c = chr $ ord 'a' + (((ord c - ord 'a') + n) `mod` 26)
  | otherwise = c

-- Vigenere-style encode using a lowercase key (a=0, b=1, ..., z=25)
vigenereEncode :: String -> String -> String
vigenereEncode key text = go text (cycle key)
  where
    go [] _ = []
    go (t:ts) ks@(k:ks')
      | isLower t = shiftChar (keyShift k) t : go ts ks'
      | otherwise = t : go ts ks
    keyShift k = ord k - ord 'a'

-- Vigenere-style decode using a lowercase key
vigenereDecode :: String -> String -> String
vigenereDecode key text = go text (cycle key)
  where
    go [] _ = []
    go (t:ts) ks@(k:ks')
      | isLower t = shiftChar (negate (keyShift k)) t : go ts ks'
      | otherwise = t : go ts ks
    keyShift k = ord k - ord 'a'

-- Validate that a key is non-empty and all lowercase letters
validateKey :: String -> Bool
validateKey k = not (null k) && all isLower k

-- Step 1: Caesar with fixed shift of 3 implemented via Step 2's mechanism
caesarEncode :: String -> String -> String
caesarEncode key text
  | validateKey key = vigenereEncode key text
  | otherwise       = error "Key must be non-empty and contain only lowercase letters."

caesarDecode :: String -> String -> String
caesarDecode key encodedText
  | validateKey key = vigenereDecode key encodedText
  | otherwise       = error "Key must be non-empty and contain only lowercase letters."

-- Generate a random lowercase key of length n
randomKey :: Int -> IO String
randomKey n = mapM (\_ -> randomRIO ('a', 'z')) [1..n]

-- Step 3: If no key is submitted (this function has no key argument),
-- generate a random key of at least 100 chars and return (key, encodedText)
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- randomKey 100
  let encoded = vigenereEncode key text
  return (key, encoded)
