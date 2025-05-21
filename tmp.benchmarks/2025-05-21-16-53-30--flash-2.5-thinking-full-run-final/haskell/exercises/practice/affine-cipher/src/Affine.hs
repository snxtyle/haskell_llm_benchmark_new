module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (intercalate)
import Prelude -- Removed 'hiding (gcd)'

-- Constants
m :: Int
m = 26

-- Helper to check if two numbers are coprime
isCoprime :: Int -> Int -> Bool
isCoprime x y = Prelude.gcd x y == 1

-- Helper to find modular multiplicative inverse (a_inv * a `mod` m = 1)
modInverse :: Int -> Int -> Maybe Int
modInverse a modulus =
    let
        -- Brute force search for x such that (a * x) mod modulus == 1
        candidates = [x | x <- [0 .. modulus - 1], (a * x) `mod` modulus == 1]
    in
        case candidates of
            (x:_) -> Just x
            []    -> Nothing -- No inverse found

-- Convert 'a'..'z' to 0..25
charToIndex :: Char -> Int
charToIndex c = ord c - ord 'a'

-- Convert 0..25 to 'a'..'z'
indexToChar :: Int -> Char
indexToChar i = chr (ord 'a' + i)

-- Normalize input: filter out non-alphanumeric, convert letters to lowercase
-- Digits are preserved, spaces and punctuation are removed.
normalizeInput :: String -> String
normalizeInput = filter (\c -> isAlpha c || isDigit c) . map toLower

-- Group string into chunks of 5, separated by spaces
groupCiphertext :: String -> String
groupCiphertext s =
    let
        go "" acc = reverse acc
        go str acc =
            let (chunk, rest) = splitAt 5 str
            in go rest (chunk : acc)
        chunks = go s []
    in
        intercalate " " chunks

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText = do
    -- Check if a and m are coprime
    if not (isCoprime a m)
        then Nothing
        else do
            let normalizedText = normalizeInput plainText
            let encryptedChars = map (\c ->
                    if isAlpha c
                        then
                            let i = charToIndex c
                                encryptedIndex = (a * i + b) `mod` m
                            in indexToChar encryptedIndex
                        else c -- Keep digits as is
                    ) normalizedText
            return $ groupCiphertext encryptedChars

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText = do
    -- Check if a and m are coprime
    if not (isCoprime a m)
        then Nothing
        else do
            a_inv <- modInverse a m -- Find modular multiplicative inverse
            -- Normalize input: removes spaces from grouped ciphertext and converts to lowercase
            let normalizedText = normalizeInput cipherText
            let decryptedChars = map (\c ->
                    if isAlpha c
                        then
                            let y = charToIndex c
                                -- Add m to (y - b) to ensure positive result before modulo
                                decryptedIndex = (a_inv * (y - b + m)) `mod` m
                            in indexToChar decryptedIndex
                        else c -- Keep digits as is
                    ) normalizedText
            return decryptedChars
