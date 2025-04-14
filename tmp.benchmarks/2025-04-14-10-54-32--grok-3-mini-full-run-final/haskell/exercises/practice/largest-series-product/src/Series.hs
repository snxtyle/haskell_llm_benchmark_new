module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan  -- Size must be non-negative
  | size > length digits = Left InvalidSpan  -- Size cannot exceed string length
  | not (all isDigit digits) = Left (InvalidDigit (head [c | c <- digits, not (isDigit c)]))  -- Find and return the first invalid digit
  | size == 0 = Right 1  -- Empty product is 1
  | otherwise = Right (maximum [product (map (fromIntegral . digitToInt) sublist) | sublist <- sublists])
  where
    sublists = [take size (drop n digits) | n <- [0..length digits - size]]
