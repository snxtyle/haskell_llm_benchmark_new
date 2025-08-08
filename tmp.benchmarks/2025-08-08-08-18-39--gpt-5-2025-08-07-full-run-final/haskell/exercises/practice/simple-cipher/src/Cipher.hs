module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr)

-- Checks if a character is a lowercase ASCII letter.
isLowerAlpha :: Char -> Bool
isLowerAlpha c = c >= 'a' && c <= 'z'

-- Convert a lowercase ASCII letter to its 0..25 index.
toIdx :: Char -> Int
toIdx c = ord c - ord 'a'

-- Convert a 0..25 index (or any Int) back to a lowercase ASCII letter.
fromIdx :: Int -> Char
fromIdx n = chr (ord 'a' + (n `mod` 26))

-- Shift a lowercase ASCII char by n (can be negative) within 'a'..'z'.
-- Non-lowercase characters are returned unchanged.
shiftN :: Int -> Char -> Char
shiftN n c
  | isLowerAlpha c = fromIdx (toIdx c + n)
  | otherwise      = c

-- Encode text using a key of lowercase letters (a=0..z).
-- If the key is empty, treat it as an infinite stream of 'a' (no shift).
caesarEncode :: String -> String -> String
caesarEncode key text =
  let ks = if null key then repeat 'a' else cycle key
      shiftVal k = if isLowerAlpha k then toIdx k else 0
  in zipWith (\k c -> if isLowerAlpha c then shiftN (shiftVal k) c else c) ks text

-- Decode text using the provided key by reversing the shifts.
caesarDecode :: String -> String -> String
caesarDecode key encodedText =
  let ks = if null key then repeat 'a' else cycle key
      shiftVal k = if isLowerAlpha k then toIdx k else 0
  in zipWith (\k c -> if isLowerAlpha c then shiftN (negate (shiftVal k)) c else c) ks encodedText

-- Generate a "random-looking" key of 100 lowercase letters (deterministic from input)
-- and encode the text with it.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  let key = generateKey 100 text
  pure (key, caesarEncode key text)

-- Deterministic key generator: produces only 'a'..'z', length n.
-- Uses a simple LCG driven by a seed derived from the input text.
generateKey :: Int -> String -> String
generateKey n text =
  take n $ map (\s -> fromIdx (fromIntegral (s `mod` 26))) (tail (iterate step seed))
  where
    -- Use Integer to avoid overflow
    a, c, m :: Integer
    a = 1103515245
    c = 12345
    m = 2147483648  -- 2^31
    step s = (a * s + c) `mod` m
    -- Seed derived from input text to vary per-call while remaining deterministic
    seed = (sum (map (toInteger . ord) text) + 987654321) `mod` m
