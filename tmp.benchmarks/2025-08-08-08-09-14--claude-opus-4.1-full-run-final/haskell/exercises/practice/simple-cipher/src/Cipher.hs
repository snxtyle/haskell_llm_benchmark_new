module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, isLower, toLower)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Helper function to normalize a character to 0-25 range
charToNum :: Char -> Int
charToNum c = ord (toLower c) - ord 'a'

-- Helper function to convert 0-25 number back to lowercase letter
numToChar :: Int -> Char
numToChar n = chr (n `mod` 26 + ord 'a')

-- Helper function to shift a character by a given amount
shiftChar :: Int -> Char -> Char
shiftChar shift c
  | isLower c = numToChar (charToNum c + shift)
  | otherwise = c

-- Helper function to encode/decode using a key
applyCipher :: (Int -> Int -> Int) -> String -> String -> String
applyCipher op key text = zipWith processChar (cycle key) text
  where
    processChar k c
      | isLower c = numToChar (charToNum c `op` charToNum k)
      | otherwise = c

caesarDecode :: String -> String -> String
caesarDecode key encodedText = applyCipher (-) key encodedText

caesarEncode :: String -> String -> String
caesarEncode key text = applyCipher (+) key text

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  -- Generate a pseudo-random key of 100 lowercase letters
  time <- getPOSIXTime
  let seed = round (time * 1000000) :: Int
  let key = generatePseudoRandomKey seed 100
  let encoded = caesarEncode key text
  return (key, encoded)

-- Helper function to generate a pseudo-random key of lowercase letters
generatePseudoRandomKey :: Int -> Int -> String
generatePseudoRandomKey seed n = take n $ map numToChar $ pseudoRandomSequence seed
  where
    -- Simple linear congruential generator
    pseudoRandomSequence s = iterate (\x -> (x * 1103515245 + 12345) `mod` (2^31)) s
