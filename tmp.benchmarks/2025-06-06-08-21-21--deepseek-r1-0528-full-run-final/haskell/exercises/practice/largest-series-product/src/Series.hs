module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (find)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | otherwise = 
      case find (not . isDigit) digits of
        Just c -> Left (InvalidDigit c)
        Nothing -> 
          if size == 0
            then Right 1
            else let windows = [ take size (drop i digits) | i <- [0 .. length digits - size] ]
                     toIntegerDigit = toInteger . digitToInt
                     products = map (product . map toIntegerDigit) windows
                 in Right (maximum products)
