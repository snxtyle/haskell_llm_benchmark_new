module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random
import Data.Char

-- | Encode text using the provided key
caesarEncode :: String -> String -> String
caesarEncode key text = zipWith encodeChar (cycle cleanKey) cleanText
  where
    cleanKey = filter isLower key
    cleanText = filter isLower text
    encodeChar k c = shiftChar (ord k - ord 'a') c

-- | Decode text using the provided key
caesarDecode :: String -> String -> String
caesarDecode key encodedText = zipWith decodeChar (cycle cleanKey) encodedText
  where
    cleanKey = filter isLower key
    decodeChar k c = shiftChar (-(ord k - ord 'a')) c

-- | Generate a random key of 100 lowercase letters and encode the text
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    gen <- newStdGen
    let key = take 100 $ randomRs ('a', 'z') gen
    let encoded = caesarEncode key (filter isLower text)
    return (key, encoded)

-- | Shift a character by n positions (wrapping around the alphabet)
shiftChar :: Int -> Char -> Char
shiftChar n c = chr $ ((ord c - ord 'a' + n) `mod` 26 + 26) `mod` 26 + ord 'a'
