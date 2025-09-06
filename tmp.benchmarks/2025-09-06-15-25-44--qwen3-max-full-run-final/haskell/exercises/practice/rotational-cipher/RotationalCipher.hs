module RotationalCipher (rotate) where

rotate :: Int -> String -> String
rotate shift = map (rotateChar shift)
  where
    rotateChar :: Int -> Char -> Char
    rotateChar n c
      | isAsciiLower c = rotateInAlphabet 'a' c n
      | isAsciiUpper c = rotateInAlphabet 'A' c n
      | otherwise = c
    
    rotateInAlphabet :: Char -> Char -> Int -> Char
    rotateInAlphabet base c n = 
      let alphabetSize = 26
          offset = fromEnum c - fromEnum base
          newIndex = (offset + n) `mod` alphabetSize
      in toEnum (fromEnum base + newIndex)
    
    isAsciiLower :: Char -> Bool
    isAsciiLower c = c >= 'a' && c <= 'z'
    
    isAsciiUpper :: Char -> Bool
    isAsciiUpper c = c >= 'A' && c <= 'Z'
