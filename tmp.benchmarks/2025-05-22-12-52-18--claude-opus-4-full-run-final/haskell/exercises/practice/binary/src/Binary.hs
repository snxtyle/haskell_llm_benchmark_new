module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs 
  | not (isValidBinary xs) = 0
  | otherwise = binaryToDecimal xs
  where
    isValidBinary :: String -> Bool
    isValidBinary = all (`elem` "01")
    
    binaryToDecimal :: String -> Int
    binaryToDecimal binary = sum $ zipWith (*) digits powers
      where
        digits = map digitToInt (reverse binary)
        powers = map (2^) [0..]
        
        digitToInt :: Char -> Int
        digitToInt '0' = 0
        digitToInt '1' = 1
        digitToInt _   = error "Invalid binary digit"
