module RotationalCipher (rotate) where

rotate :: Int -> String -> String
rotate key = map (shiftChar key)

shiftChar :: Int -> Char -> Char
shiftChar key c
  | c >= 'a' && c <= 'z' = shiftLetter 'a' c key
  | c >= 'A' && c <= 'Z' = shiftLetter 'A' c key
  | otherwise = c

shiftLetter :: Char -> Char -> Int -> Char
shiftLetter base c key = 
  let 
    offset = fromEnum c - fromEnum base
    newOffset = (offset + key) `mod` 26
  in 
    toEnum (fromEnum base + newOffset)
