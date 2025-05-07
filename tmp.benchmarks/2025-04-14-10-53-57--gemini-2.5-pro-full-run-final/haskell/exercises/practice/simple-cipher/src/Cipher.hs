module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, isLower)
import System.Random (getStdGen, randomRs)

-- Convert 'a'..'z' to 0..25
charToInt :: Char -> Int
charToInt c = ord c - ord 'a'

-- Convert 0..25 to 'a'..'z'
intToChar :: Int -> Char
intToChar i = chr (ord 'a' + i)

-- Apply a shift operation (addition or subtraction) with wrap-around
shift :: (Int -> Int -> Int) -> Char -> Char -> Char
shift op textChar keyChar
  | isLower textChar && isLower keyChar =
      let textVal = charToInt textChar
          keyVal = charToInt keyChar
          -- Apply operation and handle wrap-around using modulo 26
          -- For subtraction, adding 26 ensures the result is non-negative before modulo
          shiftedVal = (textVal `op` keyVal + 26) `mod` 26
      in intToChar shiftedVal
  | otherwise = textChar -- Pass through non-lowercase characters unchanged (though examples suggest only lowercase)

-- Core cipher function applying the shift operation along the text using the key
cipher :: (Int -> Int -> Int) -> String -> String -> String
cipher op key text = zipWith (shift op) text (cycle key)

-- Decode text using the key
caesarDecode :: String -> String -> String
caesarDecode key encodedText = cipher (-) key encodedText

-- Encode text using the key
caesarEncode :: String -> String -> String
caesarEncode key text = cipher (+) key text

-- Generate a random key and encode the text
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    gen <- getStdGen
    -- Generate a random key of exactly 100 lowercase characters
    let randomKey = take 100 $ map intToChar $ randomRs (0, 25) gen
    -- Encode the text using the generated key
    let encodedText = caesarEncode randomKey text
    return (randomKey, encodedText)
