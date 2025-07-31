module Hexadecimal (hexToInt) where

hexToInt :: String -> Int
hexToInt "" = 0
hexToInt str = hexToIntHelper (map toLower str) 0
  where
    hexToIntHelper [] acc = acc
    hexToIntHelper (c:cs) acc = 
      case charToDigit c of
        Just digit -> hexToIntHelper cs (acc * 16 + digit)
        Nothing -> 0  -- Invalid hexadecimal character
    
    charToDigit :: Char -> Maybe Int
    charToDigit c
      | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
      | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
      | otherwise = Nothing
    
    toLower :: Char -> Char
    toLower c
      | c >= 'A' && c <= 'F' = toEnum (fromEnum c + 32)
      | otherwise = c
