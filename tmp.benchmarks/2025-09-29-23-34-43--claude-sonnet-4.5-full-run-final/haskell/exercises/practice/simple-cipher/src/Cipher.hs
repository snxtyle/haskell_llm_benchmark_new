module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)

-- Decode a message using the given key
caesarDecode :: String -> String -> String
caesarDecode key encodedText = zipWith shiftBack (cycle key) encodedText
  where
    shiftBack keyChar encodedChar =
      let keyShift = charToShift keyChar
          encodedPos = charToShift encodedChar
          originalPos = (encodedPos - keyShift) `mod` 26
      in shiftToChar originalPos

-- Encode a message using the given key
caesarEncode :: String -> String -> String
caesarEncode key text = zipWith shiftForward (cycle key) text
  where
    shiftForward keyChar plainChar =
      let keyShift = charToShift keyChar
          plainPos = charToShift plainChar
          encodedPos = (plainPos + keyShift) `mod` 26
      in shiftToChar encodedPos

-- Encode a message with a randomly generated key
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- generateRandomKey 100
  let encoded = caesarEncode key text
  return (key, encoded)

-- Helper function to convert a character to its shift value (a=0, b=1, ..., z=25)
charToShift :: Char -> Int
charToShift c = fromEnum c - fromEnum 'a'

-- Helper function to convert a shift value back to a character
shiftToChar :: Int -> Char
shiftToChar n = toEnum (n + fromEnum 'a')

-- Generate a random key of the specified length with lowercase letters
generateRandomKey :: Int -> IO String
generateRandomKey len = mapM (const randomLowerChar) [1..len]
  where
    randomLowerChar = do
      n <- randomRIO (0, 25)
      return (shiftToChar n)
