module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (unfoldr)

-- Check if a and m are coprime
coprime :: Int -> Int -> Bool
coprime a m = gcd a m == 1

-- Find modular multiplicative inverse
modularInverse :: Int -> Int -> Int
modularInverse a m = head [x | x <- [1..m-1], (a*x) `mod` m == 1]

-- Encrypt a single character
encryptChar :: (Int, Int) -> Char -> Char
encryptChar (a, b) c =
  let i = ord c - ord 'a'
      j = (a * i + b) `mod` 26
  in chr (j + ord 'a')

-- Decrypt a single character
decryptChar :: (Int, Int) -> Char -> Char
decryptChar (a, b) c =
  let y = ord c - ord 'a'
      mmi = modularInverse a 26
      i = (mmi * (y - b)) `mod` 26
  in chr (i + ord 'a')

-- Group string into 5-character chunks
group5 :: String -> String
group5 s = case s of
  [] -> []
  _  -> let (h, t) = splitAt 5 s
        in if null t then h else h ++ ' ' : group5 t

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (coprime a 26) = Nothing
  | otherwise = 
      let clean = filter (\c -> isAlpha c || isDigit c) (map toLower plainText)
          encrypted = map (\c -> 
            if isAlpha c then encryptChar (a, b) c 
            else c) clean
      in Just (group5 encrypted)

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (coprime a 26) = Nothing
  | otherwise = 
      let clean = filter (\c -> isAlpha c || isDigit c) (map toLower cipherText)
          decrypted = map (\c -> 
            if isAlpha c then decryptChar (a, b) c 
            else c) clean
      in Just decrypted
