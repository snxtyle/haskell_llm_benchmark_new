module Trinary (readTri, showTri) where

-- Convert a trinary string to decimal
readTri :: String -> Int
readTri str = 
  if all isValidTriDigit str && not (null str)
    then foldl' (\acc c -> acc * 3 + charToDigit c) 0 str
    else 0
  where
    isValidTriDigit c = c == '0' || c == '1' || c == '2'
    charToDigit '0' = 0
    charToDigit '1' = 1
    charToDigit '2' = 2
    charToDigit _ = 0
    
    -- Strict left fold to ensure efficiency
    foldl' _ z [] = z
    foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

-- Convert a decimal number to trinary string
showTri :: Int -> String
showTri 0 = "0"
showTri n = reverse (go n)
  where
    go 0 = ""
    go x = let (q, r) = x `quotRem` 3
               digit = digitToChar r
           in digit `seq` (digit : go q)
    
    digitToChar 0 = '0'
    digitToChar 1 = '1'
    digitToChar 2 = '2'
    digitToChar _ = '0'
