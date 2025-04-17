module Series (Error(..), largestProduct) where

import Data.Char (digitToInt, isDigit)
import Data.List (find, tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0                    = Left InvalidSpan
  | size == 0                   = Right 1
  | size > length digits        = Left InvalidSpan
  | otherwise                   = 
      case find (not . isDigit) digits of
        Just c  -> Left (InvalidDigit c)
        Nothing ->
          let nums = map (toInteger . digitToInt) digits
              windows = [ take size t | t <- tails nums, length t >= size ]
              prods   = map product windows
          in Right (maximum prods)
