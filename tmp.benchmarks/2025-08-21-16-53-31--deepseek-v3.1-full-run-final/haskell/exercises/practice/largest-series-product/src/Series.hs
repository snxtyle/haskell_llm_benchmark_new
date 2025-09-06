module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | not (all isDigit digits) = 
      case filter (not . isDigit) digits of
        (c:_) -> Left (InvalidDigit c)
        [] -> Left InvalidSpan -- This case shouldn't happen due to the guard
  | size == 0 = Right 1
  | otherwise = Right $ maximum $ map (product . map (toInteger . digitToInt)) $ windows size digits

windows :: Int -> [a] -> [[a]]
windows n xs 
  | length xs < n = []
  | otherwise = take n xs : windows n (tail xs)
