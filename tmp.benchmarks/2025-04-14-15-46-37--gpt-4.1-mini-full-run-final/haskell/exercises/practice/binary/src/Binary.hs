module Binary (toDecimal) where

import Data.Char (digitToInt)

toDecimal :: String -> Either String Int
toDecimal xs
  | all (`elem` "01") xs = Right $ foldl (\acc x -> acc * 2 + digitToInt x) 0 xs
  | otherwise = Left "Invalid input: not a binary string"
