module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (chr, ord, isLower)
import Control.Monad (replicateM)
import System.Random (randomRIO)

-- Shift a lowercase character by n positions (wrapping around a..z).
-- Non-lowercase characters are returned unchanged.
shiftN :: Int -> Char -> Char
shiftN n c
  | isLower c = chr $ base + ((ord c - base + n) `mod` 26)
  | otherwise = c
  where
    base = ord 'a'

-- Keep only lowercase letters from the key.
-- If this results in an empty key, fall back to "a" to avoid runtime errors.
normalizeKey :: String -> String
normalizeKey k =
  let k' = filter isLower k
   in if null k' then "a" else k'

-- Encode using a (normalized) key, repeating it over the text.
encodeWith :: String -> String -> String
encodeWith key text = go (cycle (normalizeKey key)) text
  where
    go _ [] = []
    go ks (c:cs)
      | isLower c =
          let k = head ks
              restKs = tail ks
              shift = ord k - ord 'a'
           in shiftN shift c : go restKs cs
      | otherwise = c : go ks cs

-- Decode using a (normalized) key, repeating it over the text.
decodeWith :: String -> String -> String
decodeWith key text = go (cycle (normalizeKey key)) text
  where
    go _ [] = []
    go ks (c:cs)
      | isLower c =
          let k = head ks
              restKs = tail ks
              shift = ord k - ord 'a'
           in shiftN (negate shift) c : go restKs cs
      | otherwise = c : go ks cs

caesarEncode :: String -> String -> String
caesarEncode = encodeWith

caesarDecode :: String -> String -> String
caesarDecode = decodeWith

-- Generate a random lowercase key of length 100.
randomKey :: IO String
randomKey = replicateM 100 (randomRIO ('a', 'z'))

-- When no key is provided externally, generate a random key and return it with the encoded text.
-- The returned key contains only lowercase letters and is at least 100 characters long.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- randomKey
  let encoded = encodeWith key text
  pure (key, encoded)
