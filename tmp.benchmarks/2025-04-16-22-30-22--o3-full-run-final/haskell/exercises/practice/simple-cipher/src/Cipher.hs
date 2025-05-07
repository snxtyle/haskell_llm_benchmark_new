module Cipher
  ( caesarDecode
  , caesarEncode
  , caesarEncodeRandom
  ) where

import           Control.Monad   (replicateM)
import           Data.Char       (chr, isLower, ord)
import           System.Random   (randomRIO)

-- | Validate that a key contains only lowercase letters and is non‑empty
validateKey :: String -> Bool
validateKey k = not (null k) && all isLower k

-- | Convert a lowercase character to its 0‑based alphabet index
toIndex :: Char -> Int
toIndex c = ord c - ord 'a'

-- | Convert an index in 0‑25 back to a lowercase character
fromIndex :: Int -> Char
fromIndex n = chr (ord 'a' + (n `mod` 26))

-- | Shift a single character forward by /n/ places (mod 26)
shiftForward :: Int -> Char -> Char
shiftForward n c
  | isLower c = fromIndex $ toIndex c + n
  | otherwise = c

-- | Shift a single character backward by /n/ places (mod 26)
shiftBackward :: Int -> Char -> Char
shiftBackward n = shiftForward (negate n)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

-- | Decode text using the supplied key
caesarDecode :: String -> String -> String
caesarDecode key encodedText
  | validateKey key = zipWith decodeChar encodedText (cycle key)
  | otherwise       = error "invalid key"
  where
    decodeChar c k = shiftBackward (toIndex k) c

-- | Encode text using the supplied key
caesarEncode :: String -> String -> String
caesarEncode key plainText
  | validateKey key = zipWith encodeChar plainText (cycle key)
  | otherwise       = error "invalid key"
  where
    encodeChar c k = shiftForward (toIndex k) c

-- | Generate a random key (≥100 lowercase chars) and encode the text with it.
--   Returns (key, encodedText).
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom plainText = do
  key <- randomKey 100
  let encoded = caesarEncode key plainText
  return (key, encoded)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Produce a random lowercase key of at least the requested length
randomKey :: Int -> IO String
randomKey n = replicateM n randomLower
  where
    randomLower = randomRIO ('a', 'z')
