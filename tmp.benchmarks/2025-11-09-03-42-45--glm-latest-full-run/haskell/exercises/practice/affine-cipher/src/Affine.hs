module Affine (decode, encode) where

import Data.Char (chr, isAlpha, isAlphaNum, isDigit, ord, toLower)

-- | The size of the alphabet.
m :: Int
m = 26

-- | Checks if two integers are coprime (their greatest common divisor is 1).
areCoprime :: Int -> Int -> Bool
areCoprime a b = gcd a b == 1

-- | Finds the Modular Multiplicative Inverse (MMI) of a modulo m.
-- It returns Nothing if no inverse exists.
findMMI :: Int -> Int -> Maybe Int
findMMI a m = findMMI' a m 1
  where
    findMMI' a m x
      | x >= m     = Nothing
      | (a * x) `mod` m == 1 = Just x
      | otherwise = findMMI' a m (x + 1)

-- | Converts a lowercase letter to its 0-based index in the alphabet.
charToIndex :: Char -> Int
charToIndex c = ord (toLower c) - ord 'a'

-- | Converts a 0-based index to a lowercase letter.
indexToChar :: Int -> Char
indexToChar i = chr (i + ord 'a')

-- | Splits a list into sublists of a given size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Encodes a string using the Affine cipher.
-- Returns Nothing if the key 'a' is not coprime with the alphabet size.
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (areCoprime a m) = Nothing
  | otherwise = Just $ unwords $ chunksOf 5 $ map encodeChar (filter isAlphaNum plainText)
  where
    encodeChar c
      | isAlpha c = indexToChar $ ((a * charToIndex c) + b) `mod` m
      | isDigit c = c
      | otherwise = error "Unexpected character after filtering" -- Should not be reached

-- | Decodes a string that was encoded with the Affine cipher.
-- Returns Nothing if the key 'a' is not coprime with the alphabet size.
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (areCoprime a m) = Nothing
  | otherwise = case findMMI a m of
      Nothing -> Nothing -- Should not happen if coprime check passed
      Just a_inv -> Just $ map decodeChar (filter isAlphaNum cipherText)
  where
    decodeChar c
      | isAlpha c = indexToChar $ (a_inv * (charToIndex c - b)) `mod` m
      | isDigit c = c
      | otherwise = error "Unexpected character after filtering" -- Should not be reached
