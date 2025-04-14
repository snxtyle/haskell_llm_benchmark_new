module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (chr, ord, isLower)

-- Helper: convert a lowercase letter to 0-25 index
charToIndex :: Char -> Int
charToIndex c = ord c - ord 'a'

-- Helper: convert 0-25 index to lowercase letter
indexToChar :: Int -> Char
indexToChar i = chr (ord 'a' + i)

-- Helper: shift a single character by a given amount, wrapping around a-z
shiftChar :: Int -> Char -> Char
shiftChar shift c
  | isLower c = indexToChar ((charToIndex c + shift) `mod` 26)
  | otherwise = c

-- Helper: shift a single character backwards by a given amount, wrapping around a-z
unshiftChar :: Int -> Char -> Char
unshiftChar shift c
  | isLower c = indexToChar ((charToIndex c - shift) `mod` 26)
  | otherwise = c

-- Helper: convert key string to list of shifts (0-25)
keyToShifts :: String -> [Int]
keyToShifts = map charToIndex

-- Helper: repeat or truncate key shifts to match length of text
expandKey :: String -> Int -> [Int]
expandKey key len = take len (cycle (keyToShifts key))

-- Step 1 & 2: encode text with key shifts
caesarEncode :: String -> String -> String
caesarEncode key text =
  let shifts = expandKey key (length text)
  in zipWith shiftChar shifts text

-- Step 1 & 2: decode text with key shifts
caesarDecode :: String -> String -> String
caesarDecode key encodedText =
  let shifts = expandKey key (length encodedText)
  in zipWith unshiftChar shifts encodedText

-- Step 3: generate random key of length 100 lowercase letters
randomKey :: IO String
randomKey = sequence $ replicate 100 randomLowerChar
  where
    randomLowerChar = do
      n <- randomRIO (0,25)
      return $ indexToChar n

-- Step 3: encode with random key if key is empty or invalid (non-lowercase)
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- randomKey
  let encoded = caesarEncode key text
  return (key, encoded)
