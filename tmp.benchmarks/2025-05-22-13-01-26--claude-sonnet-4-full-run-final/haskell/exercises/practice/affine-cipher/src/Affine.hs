module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, toUpper, ord, chr)

-- Check if two numbers are coprime (gcd = 1)
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- Find modular multiplicative inverse using extended Euclidean algorithm
modInverse :: Int -> Int -> Maybe Int
modInverse a m
  | not (coprime a m) = Nothing
  | otherwise = Just $ ((extendedGcd a m) `mod` m + m) `mod` m
  where
    extendedGcd :: Int -> Int -> Int
    extendedGcd a 0 = 1
    extendedGcd a b = let (q, r) = divMod a b
                          x = extendedGcd b r
                          y = x - q * (extendedGcd b (r))
                      in y

-- More efficient modular inverse calculation
modInverse' :: Int -> Int -> Maybe Int
modInverse' a m
  | not (coprime a m) = Nothing
  | otherwise = Just $ head [x | x <- [1..m-1], (a * x) `mod` m == 1]

-- Convert character to 0-25 index
charToIndex :: Char -> Int
charToIndex c = ord (toLower c) - ord 'a'

-- Convert 0-25 index to character
indexToChar :: Int -> Char
indexToChar i = chr (ord 'a' + i)

-- Group string into chunks of 5 separated by spaces
groupByFive :: String -> String
groupByFive [] = []
groupByFive s = let (chunk, rest) = splitAt 5 s
                in chunk ++ if null rest then "" else " " ++ groupByFive rest

-- Filter and process input text (keep only letters and digits)
filterText :: String -> String
filterText = filter (\c -> isAlpha c || isDigit c)

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (coprime a 26) = Nothing
  | otherwise = Just $ groupByFive $ map encodeChar (filterText plainText)
  where
    encodeChar c
      | isAlpha c = let i = charToIndex c
                        encrypted = (a * i + b) `mod` 26
                    in indexToChar encrypted
      | isDigit c = c
      | otherwise = c

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText = do
  aInv <- modInverse' a 26
  return $ map (decodeChar aInv) (filter (/= ' ') cipherText)
  where
    decodeChar aInv c
      | isAlpha c = let y = charToIndex c
                        decrypted = (aInv * (y - b)) `mod` 26
                        -- Handle negative modulo
                        normalizedDecrypted = (decrypted + 26) `mod` 26
                    in indexToChar normalizedDecrypted
      | isDigit c = c
      | otherwise = c
