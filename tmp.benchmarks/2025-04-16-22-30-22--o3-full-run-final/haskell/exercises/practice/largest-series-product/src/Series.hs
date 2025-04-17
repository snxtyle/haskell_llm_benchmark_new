module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails, find)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 || size > length digits = Left InvalidSpan
  | otherwise =
      case find (not . isDigit) digits of
        Just c  -> Left (InvalidDigit c)
        Nothing ->
          if size == 0
            then Right 1
            else Right $ maximum $ map productOfChunk (chunks size digits)
  where
    productOfChunk :: String -> Integer
    productOfChunk = product . map (toInteger . digitToInt)

    chunks :: Int -> [a] -> [[a]]
    chunks n xs = [take n ys | ys <- tails xs, length ys >= n]
