module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, isLower)
import System.Random (randomRIO) -- For randomRIO
import Control.Monad (replicateM) -- For replicateM

-- Helper function to shift a character.
-- Direction: 1 for encode, -1 for decode.
-- Offset: integer shift value (0-25), derived from a key character.
-- Char: the character to shift from the plaintext or ciphertext.
-- Only shifts lowercase English letters; other characters are returned unchanged.
shiftCharInternal :: Int -> Int -> Char -> Char
shiftCharInternal direction offset char
  | isLower char =
    let valA = ord 'a'
        charVal = ord char - valA -- Convert char to 0-25 range
        
        -- Apply shift: (value + direction * offset)
        -- The result of `mod` in Haskell can be negative if the dividend is negative.
        -- (x `mod` n + n) `mod` n is a common way to ensure the result is in [0, n-1].
        shiftedVal = (charVal + direction * offset) `mod` 26
        finalShiftedVal = (shiftedVal + 26) `mod` 26 -- Ensure positive result in [0, 25]
    in chr (valA + finalShiftedVal)
  | otherwise = char

-- Processes text for encoding or decoding using a key.
-- Direction: 1 for encode, -1 for decode.
-- RawKey: the user-supplied key string.
-- Text: the text to process (plaintext or ciphertext).
processText :: Int -> String -> String -> String
processText direction rawKey text
  -- Filter the rawKey to use only lowercase letters, as per Step 3 requirements.
  | null effectiveKey = text -- If the key is empty or becomes empty after filtering (e.g., "ABC" or ""),
                             -- return the original text unchanged.
  | otherwise = zipWith applyShiftForKey (cycle effectiveKey) text
  where
    effectiveKey = filter isLower rawKey

    applyShiftForKey :: Char -> Char -> Char
    applyShiftForKey keyChar textChar =
      -- keyChar is guaranteed to be lowercase 'a'..'z' here due to `filter isLower rawKey`.
      let offset = ord keyChar - ord 'a' -- Convert key char 'a'..'z' to offset 0..25.
      in shiftCharInternal direction offset textChar

-- Decodes ciphertext using a key.
caesarDecode :: String -> String -> String
caesarDecode key encodedText = processText (-1) key encodedText

-- Encodes plaintext using a key.
caesarEncode :: String -> String -> String
caesarEncode key text = processText 1 key text

-- Encodes plaintext using a randomly generated key.
-- Returns a tuple of (randomly generated key, encoded text).
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    let randomKeyMinLength = 100 -- Key should be "at least 100" characters.
    
    -- Generate a random key consisting of lowercase English letters.
    randomKey <- replicateM randomKeyMinLength (randomRIO ('a', 'z'))
    
    -- Encode the text using the generated random key.
    -- caesarEncode will handle its key processing (which is already all lowercase)
    -- and the text (including if it's empty).
    let encodedText = caesarEncode randomKey text
    
    return (randomKey, encodedText)
