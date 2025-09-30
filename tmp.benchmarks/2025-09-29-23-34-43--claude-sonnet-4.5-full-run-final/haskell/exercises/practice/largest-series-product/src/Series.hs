module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | otherwise = case validateDigits digits of
      Just invalidChar -> Left (InvalidDigit invalidChar)
      Nothing -> Right (findLargestProduct size digits)

-- Validate that all characters are digits, return first invalid character if any
validateDigits :: String -> Maybe Char
validateDigits [] = Nothing
validateDigits (c:cs)
  | not (isDigit c) = Just c
  | otherwise = validateDigits cs

-- Find the largest product of all series with the given span
findLargestProduct :: Int -> String -> Integer
findLargestProduct 0 _ = 1  -- Empty span has product of 1
findLargestProduct size digits = maximum (map productOfSeries (allSeries size digits))

-- Get all series of the given span length
allSeries :: Int -> String -> [String]
allSeries size digits
  | size > length digits = []
  | otherwise = take size digits : allSeries size (drop 1 digits)

-- Calculate the product of a series (string of digits)
productOfSeries :: String -> Integer
productOfSeries = product . map (toInteger . digitToInt)
