module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, isAlphaNum, toLower, ord, chr)

-- | Check if two numbers are coprime.
coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

-- | Compute the modular multiplicative inverse of a modulo m.
--   Returns Nothing if the inverse does not exist.
modInv :: Int -> Int -> Maybe Int
modInv a m = let (g, x, _) = extendedGCD a m
             in if g == 1 then Just (modPositive x m) else Nothing
  where
    -- Extended Euclidean algorithm.
    extendedGCD :: Int -> Int -> (Int, Int, Int)
    extendedGCD 0 b = (b, 0, 1)
    extendedGCD a b =
      let (g, s, t) = extendedGCD (b `mod` a) a
      in (g, t - (b `div` a) * s, s)

    modPositive n m' = (n `mod` m' + m') `mod` m'

-- | Encode a single character using the affine cipher.
encodeChar :: (Int, Int) -> Char -> Char
encodeChar (a, b) c
  | isAlpha c =
      let i = ord c - ord 'a'
          e = (a * i + b) `mod` 26
      in chr (e + ord 'a')
  | otherwise = c  -- digits remain unchanged

-- | Decode a single character using the affine cipher.
decodeChar :: (Int, Int) -> Int -> Char -> Char
decodeChar (_, b) aInv c
  | isAlpha c =
      let y = ord c - ord 'a'
          d = (aInv * (y - b)) `mod` 26
      in chr (d + ord 'a')
  | otherwise = c  -- digits remain unchanged

-- | Group a string into blocks of the given size separated by spaces.
grouped :: Int -> String -> String
grouped n = unwords . chunk n
  where
    chunk _ [] = []
    chunk k xs = let (h, t) = splitAt k xs in h : chunk k t

encode :: (Int, Int) -> String -> Maybe String
encode key@(a, _) plainText
  | not (coprime a 26) = Nothing
  | otherwise =
      let cleaned = [ toLower c | c <- plainText, isAlphaNum c ]
          encoded = map (encodeChar key) cleaned
      in Just (grouped 5 encoded)

decode :: (Int, Int) -> String -> Maybe String
decode key@(a, b) cipherText
  | not (coprime a 26) = Nothing
  | otherwise = do
      aInv <- modInv a 26
      let cleaned = [ toLower c | c <- cipherText, isAlphaNum c ]
          decoded = map (decodeChar key aInv) cleaned
      return decoded
