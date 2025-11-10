module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, isLower, toLower)
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- | Shifts a lowercase character forward by a given amount, wrapping around 'z'.
-- Non-lowercase characters are returned unchanged.
shiftChar :: Char -> Int -> Char
shiftChar c k
  | isLower c = chr $ (ord c - ord 'a' + k) `mod` 26 + ord 'a'
  | otherwise = c

-- | Shifts a lowercase character backward by a given amount, wrapping around 'a'.
-- Non-lowercase characters are returned unchanged.
unshiftChar :: Char -> Int -> Char
unshiftChar c k
  | isLower c = chr $ (ord c - ord 'a' - k) `mod` 26 + ord 'a'
  | otherwise = c

-- | Encodes text using a key. Each character in the key determines the shift
-- for the corresponding character in the text. The key is repeated if it is
-- shorter than the text. This is a Vigenère cipher.
caesarEncode :: String -> String -> String
caesarEncode key text
  | null key = text -- If key is empty, return text unchanged
  | otherwise = zipWith encodeChar text (cycle key)
  where
    -- Calculate shift from key character (a=0, b=1, etc.)
    shiftFromKey k = ord (toLower k) - ord 'a'
    encodeChar p k = shiftChar p (shiftFromKey k)

-- | Decodes text that was encoded with a key.
caesarDecode :: String -> String -> String
caesarDecode key encodedText
  | null key = encodedText -- If key is empty, return text unchanged
  | otherwise = zipWith decodeChar encodedText (cycle key)
  where
    -- Calculate shift from key character (a=0, b=1, etc.)
    shiftFromKey k = ord (toLower k) - ord 'a'
    decodeChar c k = unshiftChar c (shiftFromKey k)

-- | Generates a random key of at least 100 lowercase characters and uses it
-- to encode the given text. Returns the generated key and the encoded text.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  let keyLength = 100
  -- Generate a list of random lowercase characters
  key <- replicateM keyLength (randomRIO ('a', 'z'))
  let encodedText = caesarEncode key text
  return (key, encodedText)
