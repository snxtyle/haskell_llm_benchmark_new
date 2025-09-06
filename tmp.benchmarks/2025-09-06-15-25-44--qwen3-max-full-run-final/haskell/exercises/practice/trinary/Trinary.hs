module Trinary (readTri, showTri) where

-- Convert a trinary string to its decimal equivalent
-- Invalid trinary strings are treated as 0
readTri :: String -> Integer
readTri str = if all isValidDigit str then convert 0 str else 0
  where
    isValidDigit c = c == '0' || c == '1' || c == '2'
    convert acc [] = acc
    convert acc (x:xs) = let acc' = acc * 3 + digitToInt x in acc' `seq` convert acc' xs
    digitToInt c = case c of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      _   -> 0  -- This shouldn't happen due to the isValidDigit check

-- Convert a decimal number to its trinary representation
showTri :: Integer -> String
showTri n
  | n < 0 = '-' : showTri (-n)
  | n == 0 = "0"
  | otherwise = reverse $ convert n
  where
    convert 0 = ""
    convert x = let (q, r) = x `divMod` 3 in toDigit r : convert q
    toDigit d = case d of
      0 -> '0'
      1 -> '1'
      2 -> '2'
      _ -> '0'  -- This shouldn't happen with valid input
