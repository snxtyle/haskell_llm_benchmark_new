module Hexadecimal (hexToInt) where

import Data.Char (isHexDigit, digitToInt)

hexToInt :: String -> Int
hexToInt str
  | all isHexDigit str = hexToInt' 0 str
  | otherwise = 0
  where
    hexToInt' acc [] = acc
    hexToInt' acc (x:xs) = hexToInt' (acc * 16 + digitToInt x) xs
