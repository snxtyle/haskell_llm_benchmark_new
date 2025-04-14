module RotationalCipher (rotate) where

rotate :: Int -> String -> String
rotate key str = map (shiftChar key) str
  where
    shiftChar k c
      | isLower c = toChar (fromChar c - fromChar 'a') 'a'
      | isUpper c = toChar (fromChar c - fromChar 'A') 'A'
      | otherwise = c
      where
        isLower ch = ch >= 'a' && ch <= 'z'
        isUpper ch = ch >= 'A' && ch <= 'Z'
        fromChar = fromEnum
        toChar diff base = toEnum $ fromEnum base + (diff + k) `mod` 26
