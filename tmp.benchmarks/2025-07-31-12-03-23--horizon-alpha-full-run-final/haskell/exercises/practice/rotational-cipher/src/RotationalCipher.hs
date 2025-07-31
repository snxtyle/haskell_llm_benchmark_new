module RotationalCipher (rotate) where

import Data.Char (isAlpha, isLower, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate n = map (rotateChar k)
  where
    k = n `mod` 26

rotateChar :: Int -> Char -> Char
rotateChar k c
  | isLower c = shift 'a' c
  | isUpper c = shift 'A' c
  | otherwise = c
  where
    shift base ch =
      let baseOrd = ord base
          offset = ord ch - baseOrd
          newOffset = (offset + k) `mod` 26
       in chr (baseOrd + newOffset)
