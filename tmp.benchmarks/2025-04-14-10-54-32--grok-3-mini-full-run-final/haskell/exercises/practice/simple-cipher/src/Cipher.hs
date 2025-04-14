module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)

-- Helper function to shift a single character by a given amount
shiftChar :: Char -> Int -> Char
shiftChar c n
  | c >= 'a' && c <= 'z' = toEnum $ ((fromEnum c - fromEnum 'a' + n) `mod` 26) + fromEnum 'a'
  | c >= 'A' && c <= 'Z' = toEnum $ ((fromEnum c - fromEnum 'A' + n) `mod` 26) + fromEnum 'A'
  | otherwise = c  -- Non-alphabetic characters are unchanged

-- Validate the key: Must consist only of lowercase letters
validateKey :: String -> Either String String
validateKey key
  | null key = Left "Key cannot be empty."
  | all (\c -> c >= 'a' && c <= 'z') key = Right key
  | otherwise = Left "Key must consist only of lowercase letters."

-- Get the shift value for a key character (a=0, b=1, ..., z=25)
getShift :: Char -> Int
getShift c = fromEnum c - fromEnum 'a'  -- Assumes c is lowercase

caesarEncode :: String -> String -> String
caesarEncode key text =
  case validateKey key of
    Left err -> error err  -- Error if key is invalid
    Right validKey ->
      zipWith shiftChar text (cycle shifts)  -- Apply shifts to each character
  where
    shifts = map getShift validKey  -- Convert key to list of shifts

caesarDecode :: String -> String -> String
caesarDecode key encodedText =
  case validateKey key of
    Left err -> error err  -- Error if key is invalid
    Right validKey ->
      zipWith shiftChar encodedText (cycle negativeShifts)  -- Apply negative shifts
  where
    shifts = map getShift validKey  -- Convert key to list of shifts
    negativeShifts = map (* (-1)) shifts  -- Negative for decoding

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  -- Generate a random key of exactly 100 lowercase letters
  key <- sequence [randomRIO ('a', 'z') | _ <- [1..100]]  -- List of 100 random lowercase letters
  let encoded = caesarEncode key text  -- Encode using the generated key
  return (encoded, key)  -- Return the encoded text and the key
