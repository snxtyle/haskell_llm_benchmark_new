module Trinary (readTri, showTri) where

-- Convert trinary string to decimal integer
readTri :: String -> Integer
readTri str
  | all isValidTriChar str = foldl' (\acc c -> acc * 3 + charToDigit c) 0 str
  | otherwise = 0
  where
    isValidTriChar c = c `elem` "012"
    charToDigit '0' = 0
    charToDigit '1' = 1
    charToDigit '2' = 2
    charToDigit _ = 0
    
    -- Strict left fold to ensure efficiency
    foldl' _ z [] = z
    foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

-- Convert decimal integer to trinary string
showTri :: Integer -> String
showTri 0 = "0"
showTri n = reverse $ showTri' n
  where
    showTri' 0 = ""
    showTri' x = let (q, r) = x `divMod` 3
                     digit = case r of
                               0 -> '0'
                               1 -> '1'
                               2 -> '2'
                               _ -> '0'
                 in digit : showTri' q
