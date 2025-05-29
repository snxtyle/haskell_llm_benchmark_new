module RotationalCipher (rotate) where

import Data.Char (isUpper, isLower, ord, chr)

rotate :: Int -> String -> String
rotate shift = map (rotateChar shift)
  where
    rotateChar :: Int -> Char -> Char
    rotateChar s c
      | isUpper c = shiftBase 'A' s c
      | isLower c = shiftBase 'a' s c
      | otherwise = c
    shiftBase base s c = 
      let baseOrd = ord base
          offset = (ord c - baseOrd + s) `mod` 26
       in chr (baseOrd + offset)
