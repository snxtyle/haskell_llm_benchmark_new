module RotationalCipher (rotate) where

import Data.Char

rotate :: Int -> String -> String
rotate key str = map shiftChar str
  where
    shiftChar c
      | isUpper c = rotateChar c 'A'
      | isLower c = rotateChar c 'a'
      | otherwise = c
    
    rotateChar c base = chr $ (ord c - ord base + key) `mod` 26 + ord base
