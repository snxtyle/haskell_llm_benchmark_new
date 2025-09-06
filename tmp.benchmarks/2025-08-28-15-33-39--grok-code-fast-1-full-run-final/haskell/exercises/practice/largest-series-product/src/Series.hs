module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 || size > length digits = Left InvalidSpan
  | not (all isDigit digits) = Left (InvalidDigit (head (filter (not . isDigit) digits)))
  | otherwise = Right (if size == 0 then 1 else maximum products)
  where
    nums = map digitToInt digits
    windows = [take size (drop i nums) | i <- [0 .. length nums - size]]
    products = map (product . map toInteger) windows
