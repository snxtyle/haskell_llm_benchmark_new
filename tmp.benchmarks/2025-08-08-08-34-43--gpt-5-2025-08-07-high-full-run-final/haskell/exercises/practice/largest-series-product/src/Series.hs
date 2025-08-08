module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (find, tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | otherwise =
      case find (not . isDigit) digits of
        Just c -> Left (InvalidDigit c)
        Nothing
          | size == 0 -> Right 1
          | size > length digits -> Left InvalidSpan
          | otherwise -> Right $ maximum $ map product $ slices size values
  where
    values :: [Integer]
    values = map (toInteger . digitToInt) digits

slices :: Int -> [a] -> [[a]]
slices n xs = [take n t | t <- take (length xs - n + 1) (tails xs)]
