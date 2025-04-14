module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, toUpper, chr, ord)
import Data.List (unfoldr)

-- Check if two numbers are coprime using the Euclidean algorithm
gcd' :: Int -> Int -> Int
gcd' a b = if b == 0 then abs a else gcd' b (a `mod` b)

areCoprime :: Int -> Int -> Bool
areCoprime a b = gcd' a b == 1

-- Find the modular multiplicative inverse using the extended Euclidean algorithm
modInverse :: Int -> Int -> Maybe Int
modInverse a m
  | not (areCoprime a m) = Nothing
  | otherwise = Just $ go a m 0 1
  where
    go _ 0 x _ = x `mod` m
    go a b x y =
      let q = a `div` b
          r = a `mod` b
      in go b r (y - q * x) x

-- Convert a character to its numeric value (0-25 for letters)
charToInt :: Char -> Maybe Int
charToInt c
  | isAlpha c = Just $ ord (toLower c) - ord 'a'
  | otherwise = Nothing

-- Convert a numeric value back to a character
intToChar :: Int -> Char
intToChar n = chr (ord 'a' + (n `mod` 26))

-- Split a string into groups of specified size
groupBy :: Int -> String -> String
groupBy n s = unwords $ unfoldr split s
  where
    split "" = Nothing
    split xs = Just $ splitAt n xs

-- Encode a string using the affine cipher
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (areCoprime a 26) = Nothing
  | otherwise = Just $ groupBy 5 $ map encrypt plainText'
  where
    plainText' = filter isAlpha plainText
    encrypt c = case charToInt c of
      Just x -> intToChar $ (a * x + b) `mod` 26
      Nothing -> c

-- Decode a string using the affine cipher
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (areCoprime a 26) = Nothing
  | otherwise = case modInverse a 26 of
      Nothing -> Nothing
      Just a_inv -> Just $ map decrypt cipherText'
  where
    cipherText' = filter isAlpha cipherText
    decrypt c = case charToInt c of
      Just y -> intToChar $ a_inv * (y - b) `mod` 26
      Nothing -> c
