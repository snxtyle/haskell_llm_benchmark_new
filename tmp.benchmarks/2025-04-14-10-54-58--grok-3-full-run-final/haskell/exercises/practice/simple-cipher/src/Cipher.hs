module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (chr, ord, isLower)
import System.Random (randomRIO)
import Data.List (unfoldr)

-- Caesar cipher encoding with a key
caesarEncode :: String -> String -> String
caesarEncode key text = zipWith shift text (cycle keyOffsets)
  where
    keyOffsets = map (\c -> ord c - ord 'a') key
    shift c offset = if isLower c
                     then chr $ ord 'a' + (ord c - ord 'a' + offset) `mod` 26
                     else c

-- Caesar cipher decoding with a key
caesarDecode :: String -> String -> String
caesarDecode key encodedText = zipWith shift encodedText (cycle keyOffsets)
  where
    keyOffsets = map (\c -> ord c - ord 'a') key
    shift c offset = if isLower c
                     then chr $ ord 'a' + (ord c - ord 'a' - offset) `mod` 26
                     else c

-- Generate a random key and encode the text
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- generateRandomKey 100
  return (key, caesarEncode key text)

-- Generate a random key of specified length
generateRandomKey :: Int -> IO String
generateRandomKey length = sequence $ take length $ repeat $ randomRIO ('a', 'z')
