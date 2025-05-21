module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, isLower)
import System.Random (newStdGen, randomRs, StdGen)

-- Helper to convert a lowercase character to its 0-25 integer representation
charToInt :: Char -> Int
charToInt c = ord c - ord 'a'

-- Helper to convert an integer (0-25) to a lowercase character
intToChar :: Int -> Char
intToChar i = chr (ord 'a' + i)

-- Generic function to apply a shift cipher based on a key
-- `op` is the operation to apply to the character's value and the key's shift value
-- For encoding, `op` is (+). For decoding, `op` is (-).
applyShiftWithKey :: (Int -> Int -> Int) -> String -> String -> String
applyShiftWithKey op key text =
    let
        -- Convert key characters to their 0-25 shift values
        keyShifts = map charToInt key
        keyLen = length key
    in
        [ if isLower tChar
          then let
              -- Get the corresponding key shift, cycling through the key
              kShift = keyShifts !! (idx `mod` keyLen)
              tVal = charToInt tChar
              -- Apply the operation and ensure the result wraps around 0-25
              -- The `mod 26` handles wrapping. Adding 26 before `mod 26` ensures
              -- that negative results from subtraction (for decoding) are handled correctly.
              shiftedVal = (op tVal kShift + 26) `mod` 26
          in intToChar shiftedVal
          else tChar -- Non-lowercase characters are passed through unchanged
        | (tChar, idx) <- zip text [0..]
        ]

-- Encodes a given text using a substitution cipher with the provided key.
-- The key determines the shift for each character, applied cyclically.
-- Only lowercase letters are shifted; other characters remain unchanged.
caesarEncode :: String -> String -> String
caesarEncode key text = applyShiftWithKey (+) key text

-- Decodes an encoded text using a substitution cipher with the provided key.
-- The key determines the shift for each character, applied cyclically.
-- Only lowercase letters are shifted; other characters remain unchanged.
caesarDecode :: String -> String -> String
caesarDecode key encodedText = applyShiftWithKey (-) key encodedText

-- Generates a random key and encodes the given text using that key.
-- The generated key will be at least 100 characters long and consist of
-- only lowercase letters.
-- Returns the generated key and the encoded text.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    gen <- newStdGen
    let
        -- Determine the length of the random key.
        -- It should be at least 100, and ideally as long as the text to avoid key reuse within the message.
        keyLen = max 100 (length text)
        -- Generate a list of random integers (0-25) and convert them to lowercase characters
        randomKey = take keyLen $ map intToChar (randomRs (0, 25) gen)
        -- Encode the text using the newly generated random key
        encodedText = caesarEncode randomKey text
    return (randomKey, encodedText)
