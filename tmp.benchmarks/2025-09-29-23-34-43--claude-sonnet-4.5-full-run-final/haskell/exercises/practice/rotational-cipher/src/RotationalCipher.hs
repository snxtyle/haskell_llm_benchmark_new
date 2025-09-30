module RotationalCipher (rotate) where

import Data.Char (chr, ord, isUpper, isLower)

rotate :: Int -> String -> String
rotate key = map (rotateChar key)

rotateChar :: Int -> Char -> Char
rotateChar key c
  | isUpper c = rotateUpperChar key c
  | isLower c = rotateLowerChar key c
  | otherwise = c

rotateUpperChar :: Int -> Char -> Char
rotateUpperChar key c = 
  let offset = ord c - ord 'A'
      newOffset = (offset + key) `mod` 26
  in chr (ord 'A' + newOffset)

rotateLowerChar :: Int -> Char -> Char
rotateLowerChar key c = 
  let offset = ord c - ord 'a'
      newOffset = (offset + key) `mod` 26
  in chr (ord 'a' + newOffset)
