module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | not (all isDigit digits) = Left $ InvalidDigit $ head $ filter (not . isDigit) digits
  | size == 0 = Right 1
  | otherwise = Right $ maximum $ map (product . map (toInteger . digitToInt)) $ slices size digits

-- Helper function to generate all consecutive slices of a given size
slices :: Int -> String -> [String]
slices size xs
  | size <= length xs = take size xs : slices size (tail xs)
  | otherwise = []
