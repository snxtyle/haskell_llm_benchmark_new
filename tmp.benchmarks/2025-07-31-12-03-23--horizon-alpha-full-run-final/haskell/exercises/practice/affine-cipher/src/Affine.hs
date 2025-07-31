module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, isLetter, toLower, ord, chr)
import Data.List (foldl')

-- Public API

-- Decode a ciphertext using key (a, b). Returns Nothing if 'a' is not coprime to m.
decode :: (Int, Int) -> String -> Maybe String
decode key@(a, b) cipherText = do
  invA <- modInverse a m
  let normalized = filter (\c -> isLetter c || isDigit c) $ map toLower cipherText
      decodeChar c
        | isLetter c =
            let y = charToIndex c
                x = mod' (invA * (y - b)) m
            in indexToChar x
        | isDigit c = c
        | otherwise = error "unreachable due to filtering"
  pure $ map decodeChar normalized
  where
    m = 26

-- Encode a plaintext using key (a, b). Returns Nothing if 'a' is not coprime to m.
-- Letters are encoded, digits are preserved, all output is lowercase, and
-- ciphertext is grouped in blocks of 5 separated by spaces.
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText = do
  _ <- modInverse a m -- ensure 'a' coprime to m; we don't need the inverse for encode
  let normalized = filter (\c -> isLetter c || isDigit c) $ map toLower plainText
      encodeChar c
        | isLetter c =
            let i = charToIndex c
                y = mod' (a * i + b) m
            in indexToChar y
        | isDigit c = c
        | otherwise = error "unreachable due to filtering"
      encoded = map encodeChar normalized
  pure $ group5 encoded
  where
    m = 26

-- Helpers

-- Convert lowercase letter to 0..25
charToIndex :: Char -> Int
charToIndex c = ord c - ord 'a'

-- Convert 0..25 to lowercase letter
indexToChar :: Int -> Char
indexToChar n = chr (n + ord 'a')

-- Positive modulo
mod' :: Int -> Int -> Int
mod' x m =
  let r = x `mod` m
  in if r < 0 then r + m else r

-- Group a string into chunks of 5 separated by spaces
group5 :: String -> String
group5 = unwords . chunk 5
  where
    chunk _ [] = []
    chunk n xs =
      let (h, t) = splitAt n xs
      in h : chunk n t

-- Compute gcd and multiplicative inverse if exists using Extended Euclidean Algorithm
modInverse :: Int -> Int -> Maybe Int
modInverse a m =
  let (g, x, _) = extendedGCD (mod' a m) m
  in if g == 1
       then Just (mod' x m)
       else Nothing

-- Extended Euclidean Algorithm.
-- Returns (g, x, y) such that ax + by = g = gcd(a, b)
extendedGCD :: Int -> Int -> (Int, Int, Int)
extendedGCD a 0 = (a, 1, 0)
extendedGCD a b =
  let (g, x1, y1) = extendedGCD b (a `mod` b)
      x = y1
      y = x1 - (a `div` b) * y1
  in (g, x, y)
