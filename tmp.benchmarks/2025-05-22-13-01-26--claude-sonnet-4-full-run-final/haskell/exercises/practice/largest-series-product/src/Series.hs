module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | not (all isDigit digits) = Left (InvalidDigit (head (filter (not . isDigit) digits)))
  | size == 0 = Right 1
  | otherwise = Right (maximum (map seriesProduct (allSeries size digits)))
  where
    allSeries :: Int -> String -> [String]
    allSeries n str = [take n (drop i str) | i <- [0..length str - n]]
    
    seriesProduct :: String -> Integer
    seriesProduct = product . map (toInteger . digitToInt)
