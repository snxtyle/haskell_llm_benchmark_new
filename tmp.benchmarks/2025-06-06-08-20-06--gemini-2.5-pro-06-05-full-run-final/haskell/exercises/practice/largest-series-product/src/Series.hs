module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | size == 0 = Right 1
  | otherwise =
      fmap (maximum . map product . series size) (traverse toDigit digits)
  where
    toDigit :: Char -> Either Error Integer
    toDigit c
      | isDigit c = Right (fromIntegral (digitToInt c))
      | otherwise = Left (InvalidDigit c)

    series :: Int -> [a] -> [[a]]
    series n xs = map (take n) . take (length xs - n + 1) . tails $ xs
