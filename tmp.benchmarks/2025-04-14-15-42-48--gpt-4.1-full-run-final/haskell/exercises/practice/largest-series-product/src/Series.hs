module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)
import Data.Either (lefts)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | size == 0 = Right 1
  | otherwise =
      case lefts parsedDigits of
        (err:_) -> Left err
        [] -> Right $ maximum $ map productOfSeries (series parsedDigits')
  where
    parsedDigits = map parseDigit digits
    parsedDigits' = rights parsedDigits

    parseDigit c
      | isDigit c = Right (toInteger (digitToInt c))
      | otherwise = Left (InvalidDigit c)

    rights :: [Either a b] -> [b]
    rights = foldr (\x acc -> case x of Right v -> v:acc; _ -> acc) []

    series xs = [take size t | t <- tails xs, length t >= size]

    productOfSeries = product

