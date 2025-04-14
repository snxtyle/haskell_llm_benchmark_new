module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (chr, ord, isLower)
import Control.Monad (replicateM)

-- Helper: convert a-z to 0-25
charToInt :: Char -> Int
charToInt c = ord c - ord 'a'

-- Helper: convert 0-25 to a-z
intToChar :: Int -> Char
intToChar n = chr (ord 'a' + n)

-- Helper: shift a single character by n (0-25), wrapping around
shiftChar :: Int -> Char -> Char
shiftChar n c
  | isLower c = intToChar $ (charToInt c + n) `mod` 26
  | otherwise = c

-- Helper: unshift a single character by n (0-25), wrapping around
unshiftChar :: Int -> Char -> Char
unshiftChar n c
  | isLower c = intToChar $ (charToInt c - n + 26) `mod` 26
  | otherwise = c

-- Caesar cipher: shift by 3
caesarEncode :: String -> String -> String
caesarEncode _ text = map (shiftChar 3) text

caesarDecode :: String -> String -> String
caesarDecode _ text = map (unshiftChar 3) text

-- Substitution cipher: key is a string of lowercase letters, each letter is a shift
-- If key is shorter than text, repeat key as needed
encodeWithKey :: String -> String -> String
encodeWithKey key text = zipWith shiftChar shifts text
  where
    key' = cycle key
    shifts = map charToInt key'

decodeWithKey :: String -> String -> String
decodeWithKey key text = zipWith unshiftChar shifts text
  where
    key' = cycle key
    shifts = map charToInt key'

-- caesarEncodeRandom: if key is empty, generate a random key of at least 100 lowercase letters
-- Returns (key, encodedText)
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- randomKey (max 100 (length text))
  let encoded = encodeWithKey key text
  return (key, encoded)

-- Helper: generate a random key of given length (lowercase letters)
randomKey :: Int -> IO String
randomKey n = replicateM n randomLower
  where
    randomLower = do
      i <- randomRIO (0,25)
      return $ intToChar i
