module Affine (decode, encode) where

import Data.Char  (isAlpha, isDigit, toLower, ord, chr)
import Data.Maybe (mapMaybe)

-- Size of the Roman alphabet
m :: Int
m = 26

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Encode plaintext with an affine cipher.
--   Returns Nothing when the key is invalid (i.e. ‘a’ is not coprime with 26).
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | gcd a m /= 1 = Nothing
  | otherwise    = Just . unwords . chunksOf 5 $ mapMaybe encodeChar plainText
  where
    encodeChar :: Char -> Maybe Char
    encodeChar c
      | isAlpha c =
          let i = charToIndex c
              e = (a * i + b) `mod` m
          in Just (indexToChar e)
      | isDigit c = Just c          -- digits are kept as‑is
      | otherwise = Nothing         -- ignore spaces / punctuation

-- | Decode ciphertext produced by an affine cipher.
--   Returns Nothing when the key is invalid (i.e. ‘a’ is not coprime with 26).
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText = do
  inv <- modInv a m                     -- fails when a and m are not coprime
  let decodeChar c
        | isAlpha c =
            let y = charToIndex c
                d = (inv * (y - b)) `mod` m
            in Just (indexToChar d)
        | isDigit c = Just c
        | otherwise = Nothing
  pure $ mapMaybe decodeChar cipherText

-------------------------------------------------------------------------------
-- Internal helpers
-------------------------------------------------------------------------------

-- Modular multiplicative inverse of ‘a’ modulo ‘modulus’, if it exists.
modInv :: Int -> Int -> Maybe Int
modInv a modulus =
  let a' = a `mod` modulus
  in case [ x | x <- [1 .. modulus - 1], (a' * x) `mod` modulus == 1 ] of
       (x:_) -> Just x
       _     -> Nothing

-- Convert alphabetic character (case‑insensitive) to 0‑25 index.
charToIndex :: Char -> Int
charToIndex c = ord (toLower c) - ord 'a'

-- Convert 0‑25 index to lowercase alphabetic character.
indexToChar :: Int -> Char
indexToChar i = chr (i + ord 'a')

-- Split a list into chunks of size ‘n’.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0    = error "chunksOf: chunk size must be positive"
  | null xs   = []
  | otherwise = take n xs : chunksOf n (drop n xs)
