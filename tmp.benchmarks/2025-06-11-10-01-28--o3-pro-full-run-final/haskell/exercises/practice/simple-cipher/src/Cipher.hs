module Cipher
  ( caesarDecode
  , caesarEncode
  , caesarEncodeRandom
  ) where

import           Control.Monad      (replicateM)
import           Data.Char          (chr, isLower, ord)
import           System.Random      (randomRIO)

-------------------------------------------------------------------------------
-- Helper utilities
-------------------------------------------------------------------------------

alphabetSize :: Int
alphabetSize = 26

-- | Convert a lower-case character to its 0-based alphabet index.
charToIndex :: Char -> Int
charToIndex c = ord c - ord 'a'

-- | Convert an index (0-25) back to a lower-case character.
indexToChar :: Int -> Char
indexToChar i = chr (ord 'a' + i `mod` alphabetSize)

-- | Shift a single lower-case character by @n@ positions (mod 26).
shiftChar :: Int -> Char -> Char
shiftChar n c = indexToChar (charToIndex c + n)

-- | Ensure the key is an infinite stream by cycling it.
cycleKey :: String -> [Char]
cycleKey [] = error "Key must contain at least one lower-case letter."
cycleKey ks = cycle ks

-- | Validate that all characters of a string are lower-case letters.
validateLower :: String -> Bool
validateLower = all isLower

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Encode plaintext using a Vigenère-style substitution cipher.
caesarEncode :: String   -- ^ key (must be lower-case letters)
             -> String   -- ^ plaintext (expected lower-case letters)
             -> String
caesarEncode key txt
  | not (validateLower key) = error "Key must contain only lower-case letters."
  | otherwise               = zipWith encodeChar (cycleKey key) txt
  where
    encodeChar k p
      | isLower p = shiftChar (charToIndex k) p
      | otherwise = p                  -- leave non-lower-case unchanged

-- | Decode cipher text that was produced by 'caesarEncode'.
caesarDecode :: String   -- ^ key used for encoding
             -> String   -- ^ cipher text
             -> String
caesarDecode key txt
  | not (validateLower key) = error "Key must contain only lower-case letters."
  | otherwise               = zipWith decodeChar (cycleKey key) txt
  where
    decodeChar k c
      | isLower c = shiftChar (-charToIndex k) c
      | otherwise = c

-- | Encodes a message with a freshly generated random key of at least
--   100 lower-case letters, returning the key together with the cipher.
caesarEncodeRandom :: String                     -- ^ plaintext
                   -> IO (String, String)        -- ^ (key, cipher text)
caesarEncodeRandom txt = do
    key <- randomKey 100
    pure (key, caesarEncode key txt)
  where
    randomKey :: Int -> IO String
    randomKey n = replicateM n randomLower

    randomLower :: IO Char
    randomLower = do
      i <- randomRIO (0, alphabetSize - 1)
      pure (indexToChar i)
