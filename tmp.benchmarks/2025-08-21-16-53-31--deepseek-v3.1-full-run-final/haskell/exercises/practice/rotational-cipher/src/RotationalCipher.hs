module RotationalCipher (rotate) where

import Data.Char (isLetter, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate shift = map (rotateChar shift)

rotateChar :: Int -> Char -> Char
rotateChar shift c
    | not (isLetter c) = c
    | isUpper c = shiftBase 'A' c
    | otherwise = shiftBase 'a' c
    where
        shiftBase base char = 
            let baseCode = ord base
                charCode = ord char
                shifted = (charCode - baseCode + shift) `mod` 26 + baseCode
            in chr shifted
