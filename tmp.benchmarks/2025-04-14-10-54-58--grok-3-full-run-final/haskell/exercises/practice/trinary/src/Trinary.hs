{-# LANGUAGE BangPatterns #-}

module Trinary (readTri, showTri) where

-- Convert a trinary string to a decimal number
readTri :: String -> Integer
readTri s = if validTrinary s then toDecimal s else 0
  where
    validTrinary = all (`elem` "012")
    toDecimal = foldl (\acc d -> acc * 3 + digitToInt d) 0
    digitToInt c = fromIntegral (fromEnum c - fromEnum '0')

-- Convert a decimal number to a trinary string
showTri :: Integer -> String
showTri 0 = "0"
showTri n = reverse $ go n
  where
    go !x
      | x == 0    = ""
      | otherwise = let (q, r) = x `divMod` 3 in intToDigit r : go q
    intToDigit d = toEnum (fromEnum '0' + fromIntegral d)
