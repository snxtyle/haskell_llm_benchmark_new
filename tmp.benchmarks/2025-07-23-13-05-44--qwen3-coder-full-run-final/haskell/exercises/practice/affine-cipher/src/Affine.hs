module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower)

-- | Check if two numbers are coprime
coprime :: Int -> Int -> Bool
coprime a m = gcd a m == 1

-- | Find modular multiplicative inverse
mmi :: Int -> Int -> Maybe Int
mmi a m = 
  if coprime a m
  then Just $ head [x | x <- [1..m-1], (a * x) `mod` m == 1]
  else Nothing

-- | Convert character to index (0-25)
charToIndex :: Char -> Int
charToIndex c = fromEnum c - fromEnum 'a'

-- | Convert index to character
indexToChar :: Int -> Char
indexToChar i = toEnum (i + fromEnum 'a')

-- | Encrypt a single character
encryptChar :: Int -> Int -> Char -> Char
encryptChar a b c = indexToChar ((a * charToIndex c + b) `mod` 26)

-- | Decrypt a single character
decryptChar :: Int -> Int -> Char -> Maybe Char
decryptChar a b c = do
  inv <- mmi a 26
  return $ indexToChar ((inv * (charToIndex c - b)) `mod` 26)

-- | Process text for encryption (remove non-alphanumeric, lowercase)
processPlainText :: String -> String
processPlainText = filter (\c -> isAlpha c || isDigit c) . map toLower

-- | Process text for decryption (remove spaces)
processCipherText :: String -> String
processCipherText = filter (/= ' ')

-- | Group string into chunks of 5
groupText :: String -> String
groupText [] = []
groupText s = 
  let (chunk, rest) = splitAt 5 s
  in chunk ++ if null rest then [] else ' ' : groupText rest

-- | Encode using affine cipher
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText = do
  _ <- mmi a 26  -- Check if a and 26 are coprime
  let processed = processPlainText plainText
  let encryptedChars = map (\c -> if isAlpha c then encryptChar a b c else c) processed
  return $ groupText encryptedChars

-- | Decode affine cipher
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText = do
  _ <- mmi a 26  -- Check if a and 26 are coprime
  let processed = processCipherText cipherText
  decryptedChars <- sequence $ map (\c -> if isAlpha c then decryptChar a b c else Just c) processed
  return decryptedChars
