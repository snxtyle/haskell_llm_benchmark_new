module Affine (decode, encode) where

import           Data.Char (chr, isAlpha, isDigit, ord, toLower)
import           Data.List (intercalate)

-- | Length of the Roman alphabet we work with.
alphabetSize :: Int
alphabetSize = 26

-- | Public ────────────────────────────────────────────────────────────────────

encode :: (Int, Int) -> String -> Maybe String
encode key@(a, b) plainText
  | not (coprime a alphabetSize) = Nothing
  | otherwise                    = Just . groupFive $ map encodeChar cleaned
  where
    cleaned = [ c | c <- plainText, isAlpha c || isDigit c ]
    encodeChar c
      | isAlpha c = intToChar $ (a * charToInt c + b) `mod` alphabetSize
      | otherwise = c  -- digit, leave unchanged

decode :: (Int, Int) -> String -> Maybe String
decode key@(a, b) cipherText
  | not (coprime a alphabetSize) = Nothing
  | otherwise                    =
      case modInverse a alphabetSize of
        Nothing     -> Nothing  -- Should not happen because we already check coprimality
        Just aInv   -> Just $ map (decodeChar aInv) cleaned
  where
    cleaned = [ c | c <- cipherText, isAlpha c || isDigit c ]
    decodeChar aInv c
      | isAlpha c = intToChar $ (aInv * (charToInt c - b)) `mod` alphabetSize
      | otherwise = c  -- digit, leave unchanged

-- | Helpers ───────────────────────────────────────────────────────────────────

-- Convert alphabet character to 0‑based numeric index.
charToInt :: Char -> Int
charToInt c = ord (toLower c) - ord 'a'

-- Convert numeric index (0‑based) back to alphabet character.
intToChar :: Int -> Char
intToChar n = chr (n + ord 'a')

-- Group a string into chunks of five characters separated by spaces.
groupFive :: String -> String
groupFive = intercalate " " . chunksOf 5
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Check whether two integers are coprime.
coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

-- Extended greatest common divisor.
egcd :: Int -> Int -> (Int, Int, Int)
egcd 0 b = (b, 0, 1)
egcd a b =
  let (g, s, t) = egcd (b `mod` a) a
  in (g, t - (b `div` a) * s, s)

-- Modular multiplicative inverse of a modulo m, if it exists.
modInverse :: Int -> Int -> Maybe Int
modInverse a m =
  let (g, x, _) = egcd a m
  in if g == 1
       then Just (x `mod` m)
       else Nothing
