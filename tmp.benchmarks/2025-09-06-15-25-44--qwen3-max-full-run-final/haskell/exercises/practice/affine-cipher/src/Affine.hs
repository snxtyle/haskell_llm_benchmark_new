module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)

-- Check if two numbers are coprime (gcd == 1)
areCoprime :: Int -> Int -> Bool
areCoprime a m = gcd a m == 1

-- Find modular multiplicative inverse
findMMI :: Int -> Int -> Maybe Int
findMMI a m = case [x | x <- [1..m-1], (a * x) `mod` m == 1] of
    [] -> Nothing
    (x:_) -> Just x

-- Normalize character to index (a=0, b=1, ...)
charToIndex :: Char -> Int
charToIndex c = ord (toLower c) - ord 'a'

-- Convert index back to character
indexToChar :: Int -> Char
indexToChar i = chr (i + ord 'a')

-- Clean input: keep only letters and digits, convert to lowercase
cleanInput :: String -> String
cleanInput = filter (\c -> isAlpha c || isDigit c) . map toLower

-- Encrypt a single character
encryptChar :: Int -> Int -> Int -> Char -> Char
encryptChar a b m c
    | isDigit c = c
    | otherwise = indexToChar $ (a * charToIndex c + b) `mod` m

-- Decrypt a single character
decryptChar :: Int -> Int -> Int -> Char -> Maybe Char
decryptChar a b m c
    | isDigit c = Just c
    | otherwise = do
        aInv <- findMMI a m
        let y = charToIndex c
        let x = (aInv * (y - b)) `mod` m
        Just $ indexToChar ((x + m) `mod` m)

-- Group string into chunks of n characters
groupString :: Int -> String -> String
groupString n [] = []
groupString n s = take n s ++ if length s > n then ' ' : groupString n (drop n s) else []

-- Constants
alphabetSize :: Int
alphabetSize = 26

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
    | not (areCoprime a alphabetSize) = Nothing
    | otherwise = Just $ groupString 5 encrypted
    where
        cleaned = cleanInput plainText
        encrypted = map (encryptChar a b alphabetSize) cleaned

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
    | not (areCoprime a alphabetSize) = Nothing
    | otherwise = do
        aInv <- findMMI a alphabetSize
        let cleaned = filter (\c -> isAlpha c || isDigit c) cipherText
        decrypted <- sequence $ map (decryptChar a b alphabetSize) cleaned
        return decrypted
