module RotationalCipher (rotate) where

import Data.Char (isLower, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate key = map (rotateChar key)

rotateChar :: Int -> Char -> Char
rotateChar key c
    | isLower c = rotateFrom 'a' key c
    | isUpper c = rotateFrom 'A' key c
    | otherwise = c
    where
        rotateFrom :: Char -> Int -> Char -> Char
        rotateFrom base k char =
            let offset = ord char - ord base
                rotatedOffset = (offset + k) `mod` 26
            in chr (ord base + rotatedOffset)
