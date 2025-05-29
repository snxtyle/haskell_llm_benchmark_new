module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | not (all isDigit digits) = 
      let nonDigit = head [c | c <- digits, not (isDigit c)]
      in Left (InvalidDigit nonDigit)
  | size == 0 = Right 1
  | otherwise = 
      let digitsInt = map (fromIntegral . digitToInt) digits
          products = [product (take size (drop i digitsInt)) 
                     | i <- [0 .. length digits - size]]
      in Right (maximum products)
