module RotationalCipher (rotate) where

import Data.Char (isAsciiLower, isAsciiUpper, ord, chr)

rotate :: Int -> String -> String
rotate key = map (rotateChar key)

rotateChar :: Int -> Char -> Char
rotateChar key c
  | isAsciiLower c = rotateLetter 'a' key c
  | isAsciiUpper c = rotateLetter 'A' key c
  | otherwise      = c

rotateLetter :: Char -> Int -> Char -> Char
rotateLetter base key char =
  let
    -- Calculate the position of the character relative to the base ('a' or 'A')
    pos = ord char - ord base
    -- Apply the rotation, wrapping around using modulo 26
    rotatedPos = (pos + key) `mod` 26
    -- Convert the new position back to a character
  in
    chr (ord base + rotatedPos)

