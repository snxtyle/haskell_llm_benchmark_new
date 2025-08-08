module Trinary (readTri, showTri) where

-- Convert a trinary string to decimal
readTri :: String -> Integer
readTri str
  | isValidTrinary str = convertToDecimal str
  | otherwise = 0
  where
    -- Check if string contains only valid trinary digits (0, 1, 2)
    isValidTrinary :: String -> Bool
    isValidTrinary [] = True
    isValidTrinary (c:cs) = c `elem` "012" && isValidTrinary cs
    
    -- Convert valid trinary string to decimal using first principles
    convertToDecimal :: String -> Integer
    convertToDecimal s = go (reverse s) 0 0
      where
        go :: String -> Integer -> Integer -> Integer
        go [] _ acc = acc
        go (d:ds) power acc = 
          let digit = fromIntegral (fromEnum d - fromEnum '0')
              value = digit * (3 ^ power)
              newAcc = acc + value
          in newAcc `seq` go ds (power + 1) newAcc

-- Convert a decimal number to trinary string
showTri :: Integer -> String
showTri 0 = "0"
showTri n
  | n < 0 = error "Negative numbers not supported"
  | otherwise = reverse (go n)
  where
    go :: Integer -> String
    go 0 = ""
    go x = 
      let (q, r) = x `quotRem` 3
          digit = toEnum (fromInteger r + fromEnum '0')
      in digit : go q
