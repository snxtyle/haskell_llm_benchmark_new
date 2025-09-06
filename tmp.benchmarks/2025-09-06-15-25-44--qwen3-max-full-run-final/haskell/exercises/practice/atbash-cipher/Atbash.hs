module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower)

-- Create the Atbash mapping: a->z, b->y, c->x, etc.
atbashMap :: Char -> Char
atbashMap c
  | isAlpha c = chr (219 - ord (toLower c))  -- 219 = ord 'a' + ord 'z'
  | otherwise = c

-- Helper function to convert a character using the Atbash cipher
atbashChar :: Char -> Maybe Char
atbashChar c
  | isAlpha c = Just (atbashMap c)
  | isDigit c = Just c
  | otherwise = Nothing

-- Encode a string using the Atbash cipher
encode :: String -> String
encode plainText = formatGroups $ map atbashMap $ filter keepChar plainText
  where
    keepChar c = isAlpha c || isDigit c
    formatGroups [] = []
    formatGroups s
      | length s <= 5 = s
      | otherwise = take 5 s ++ " " ++ formatGroups (drop 5 s)

-- Decode a string using the Atbash cipher
decode :: String -> String
decode cipherText = map atbashMap $ filter keepChar cipherText
  where
    keepChar c = isAlpha c || isDigit c

-- Helper functions for character conversion
ord :: Char -> Int
ord = fromEnum

chr :: Int -> Char
chr = toEnum
