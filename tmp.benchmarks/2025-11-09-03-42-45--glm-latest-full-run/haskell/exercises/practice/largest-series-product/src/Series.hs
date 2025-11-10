module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (find, product, maximum)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | otherwise = case find (not . isDigit) digits of
                  Just c -> Left (InvalidDigit c)
                  Nothing -> if size == 0
                             then Right 1
                             else
                                let numDigits = map (fromIntegral . digitToInt) digits
                                    allProducts = map product (seriesOf size numDigits)
                                in Right (maximum allProducts)
  where
    seriesOf :: Int -> [a] -> [[a]]
    seriesOf n xs
      | n > length xs = []
      | otherwise = take n xs : seriesOf n (tail xs)
