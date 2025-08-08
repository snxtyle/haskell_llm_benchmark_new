module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | not (null badChars) = Left (InvalidDigit (head badChars))
  | size > length digits = Left InvalidSpan
  | size == 0 = Right 1
  | otherwise = Right $ maximum $ map productOf (windows size digits)
  where
    badChars = filter (not . isDigit) digits

    productOf :: String -> Integer
    productOf = foldr (\c acc -> toInteger (digitToInt c) * acc) 1

    windows :: Int -> [a] -> [[a]]
    windows n xs
      | n > length xs = []
      | otherwise = take n xs : windows n (tail xs)
