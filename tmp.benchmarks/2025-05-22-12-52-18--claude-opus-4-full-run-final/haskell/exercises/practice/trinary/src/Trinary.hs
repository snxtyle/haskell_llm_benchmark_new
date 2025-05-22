module Trinary (readTri, showTri) where

-- Convert a trinary string to decimal
readTri :: Integral a => String -> a
readTri s = if isValidTrinary s
            then fromInteger (readTri' (reverse s) 0 0)
            else 0
  where
    -- Check if string contains only valid trinary digits (0, 1, 2)
    isValidTrinary :: String -> Bool
    isValidTrinary [] = True
    isValidTrinary (c:cs) = c `elem` "012" && isValidTrinary cs
    
    -- Convert valid trinary string to decimal
    -- Uses accumulator and position for efficiency
    readTri' :: String -> Integer -> Integer -> Integer
    readTri' [] acc _ = acc
    readTri' (c:cs) acc pos = 
        let digit = fromIntegral (fromEnum c - fromEnum '0')
            value = digit * (3 ^ pos)
            newAcc = acc + value
        in newAcc `seq` readTri' cs newAcc (pos + 1)

-- Convert a decimal number to trinary string
showTri :: Integral a => a -> String
showTri 0 = "0"
showTri n = reverse (showTri' (toInteger n))
  where
    showTri' :: Integer -> String
    showTri' 0 = ""
    showTri' x = 
        let (q, r) = x `quotRem` 3
            digit = toEnum (fromIntegral r + fromEnum '0')
        in digit : showTri' q
