module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs
  | null xs = 0
  | not (all isBinaryDigit xs) = 0
  | otherwise = convertBinary xs

isBinaryDigit :: Char -> Bool
isBinaryDigit c = c == '0' || c == '1'

convertBinary :: String -> Int
convertBinary xs = sum $ zipWith (*) powers digits
  where
    digits = map charToInt (reverse xs)
    powers = map (2^) ([0..] :: [Int])
    charToInt '0' = 0
    charToInt '1' = 1
    charToInt _ = 0
