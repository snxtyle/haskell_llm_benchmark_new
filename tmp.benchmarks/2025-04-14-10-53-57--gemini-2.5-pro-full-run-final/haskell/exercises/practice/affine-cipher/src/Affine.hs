module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, isLower, isSpace, toLower, ord, chr)
import Data.List (intercalate)

-- Constants
m :: Int
m = 26 -- Alphabet size (Roman alphabet)

aOffset :: Int
aOffset = ord 'a' -- ASCII offset for 'a'

-- === Helper Functions ===

-- Euclidean algorithm for Greatest Common Divisor
gcd' :: Int -> Int -> Int
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

-- Check if 'a' is coprime with the alphabet size 'm'
isCoprime :: Int -> Bool
isCoprime a = gcd' a m == 1

-- Extended Euclidean Algorithm: finds integers x, y such that ax + my = gcd(a, m)
-- Returns tuple (gcd, x, y)
extendedGcd :: Int -> Int -> (Int, Int, Int)
extendedGcd a 0 = (abs a, signum a, 0) -- Base case: gcd is |a|, x is sign(a), y is 0
extendedGcd a b = (g, y, x - (a `div` b) * y) -- Recursive step
  where
    (g, x, y) = extendedGcd b (a `mod` b) -- Call with smaller numbers

-- Modular Multiplicative Inverse: finds x such that ax ≡ 1 (mod m)
-- Returns Maybe Int, as inverse only exists if a and m are coprime
mmi :: Int -> Maybe Int
mmi a =
  let (g, x, _) = extendedGcd a m -- Calculate gcd and Bezout coefficient x
  in if g == 1 -- Inverse exists only if gcd is 1
     then Just (x `mod` m) -- Return x mod m to ensure it's in [0..m-1]
     else Nothing          -- Otherwise, no inverse exists

-- Convert a lowercase character to its 0-based index (a=0, b=1, ...)
charToIndex :: Char -> Maybe Int
charToIndex c
  | isLower c = Just (ord c - aOffset)
  | otherwise = Nothing -- Only handle lowercase letters

-- Convert a 0-based index back to a lowercase character
indexToChar :: Int -> Char
indexToChar i = chr (aOffset + i)

-- Clean input string for encoding: convert to lowercase, keep only letters and digits
cleanEncodeInput :: String -> String
cleanEncodeInput = filter (\c -> isAlpha c || isDigit c) . map toLower

-- Clean input string for decoding: keep only letters and digits (case irrelevant here)
cleanDecodeInput :: String -> String
cleanDecodeInput = filter (\c -> isAlpha c || isDigit c)

-- Grouping function: groups a string into chunks of size n, separated by spaces
groupOutput :: Int -> String -> String
groupOutput n = intercalate " " . chunksOf' n
  where
    chunksOf' :: Int -> [a] -> [[a]]
    chunksOf' _ [] = [] -- Base case: empty list
    chunksOf' k xs = take k xs : chunksOf' k (drop k xs) -- Take first chunk, recurse on rest

-- === Core Functions ===

-- Encode plaintext using the Affine cipher
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  -- Check for coprimality before proceeding
  | not (isCoprime a) = Nothing
  | otherwise = Just $ groupOutput 5 $ concatMap processChar cleanedText
  where
    cleanedText = cleanEncodeInput plainText -- Prepare the input string
    -- Process a single character for encoding
    processChar :: Char -> String
    processChar c
      | isDigit c = [c] -- Pass digits through unchanged
      | isLower c = case charToIndex c of -- Encrypt lowercase letters
                      Just i -> [indexToChar ((a * i + b) `mod` m)] -- Apply encryption formula
                      Nothing -> "" -- Should not happen for isLower chars after cleaning
      | otherwise = "" -- Should not happen after cleanEncodeInput

-- Decode ciphertext using the Affine cipher
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText = do -- Use Maybe monad for potential MMI failure
  aInv <- mmi a -- Calculate Modular Multiplicative Inverse (checks coprimality)
  let cleanedText = cleanDecodeInput cipherText -- Prepare the input string
  -- Apply decryption to the cleaned text
  Just $ concatMap (processChar aInv) cleanedText
  where
    -- Process a single character for decoding
    processChar :: Int -> Char -> String
    processChar aInv c
      | isDigit c = [c] -- Pass digits through unchanged
      | isAlpha c = case charToIndex (toLower c) of -- Decrypt letters (case-insensitive)
                      Just y -> [indexToChar ((aInv * (y - b)) `mod` m)] -- Apply decryption formula
                      Nothing -> "" -- Should not happen for isAlpha chars
      | otherwise = "" -- Should not happen after cleanDecodeInput
