module Series (Error(..), largestProduct) where

import Data.Char (digitToInt, isDigit)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | otherwise = case traverse toDigit digits of
      Left err -> Left err
      Right ds -> Right $ maximumProduct size ds
  where
    toDigit c
      | isDigit c = Right (toInteger (digitToInt c))
      | otherwise = Left (InvalidDigit c)

    maximumProduct 0 _ = 1
    maximumProduct n ds = maximum $ map product $ filter ((== n) . length) $ map (take n) (tails ds)
