module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (ord, chr, isAsciiLower, toLower)

-- Helper function to shift a character by a given amount
shiftChar :: Int -> Char -> Char
shiftChar shift c
    | isAsciiLower c = let base = ord 'a'
                           pos = ord c - base
                           newPos = (pos + shift) `mod` 26
                       in chr (base + newPos)
    | otherwise = c

-- Helper function to unshift a character by a given amount
unshiftChar :: Int -> Char -> Char
unshiftChar shift c
    | isAsciiLower c = let base = ord 'a'
                           pos = ord c - base
                           newPos = (pos - shift) `mod` 26
                       in chr (base + newPos)
    | otherwise = c

-- Apply a function to each character pair from text and key
applyWithKey :: (Int -> Char -> Char) -> String -> String -> String
applyWithKey f key text = zipWith (\k c -> f (shiftAmount k) c) (cycle key) text
  where
    shiftAmount k = ord k - ord 'a'

caesarDecode :: String -> String -> String
caesarDecode key encodedText = applyWithKey unshiftChar (normalizeKey key) encodedText

caesarEncode :: String -> String -> String
caesarEncode key text = applyWithKey shiftChar (normalizeKey key) text

-- Generate a random key of at least 100 lowercase characters
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    -- Generate a random key of length 100 or text length (whichever is larger)
    let keyLength = max 100 (length text)
    key <- mapM (\_ -> randomRIO ('a', 'z')) [1..keyLength]
    let encoded = caesarEncode key text
    return (key, encoded)

-- Ensure key contains only lowercase letters
normalizeKey :: String -> String
normalizeKey key
    | null key = "a"  -- Default key if empty
    | otherwise = map toLower $ filter isAsciiLower key
