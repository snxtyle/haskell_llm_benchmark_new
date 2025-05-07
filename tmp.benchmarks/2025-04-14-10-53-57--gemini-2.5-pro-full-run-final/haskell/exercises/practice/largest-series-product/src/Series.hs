module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)
import Data.Traversable (traverse)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

-- Helper function to validate a single character and convert it to an Integer
validateChar :: Char -> Either Error Integer
validateChar c
  | isDigit c = Right (toInteger (digitToInt c))
  | otherwise = Left (InvalidDigit c)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  -- Validate span size
  | size < 0 = Left InvalidSpan
  -- Handle edge case for size 0
  | size == 0 = Right 1
  -- Process valid, non-zero size
  | otherwise =
      -- Validate all characters and convert string to list of Integers
      case traverse validateChar digits of
        Left err -> Left err -- Return InvalidDigit error if found
        Right nums ->
          let n = length nums
          -- Check if span size is valid relative to the number of digits
          in if size > n
               then Left InvalidSpan
               else
                 -- Generate all series of the given size
                 -- tails nums generates all suffixes: [[1,2,3], [2,3], [3], []]
                 -- map (take size) takes the first 'size' elements of each suffix
                 -- take (n - size + 1) ensures we only take complete series
                 let series = take (n - size + 1) $ map (take size) (tails nums)
                     -- Calculate the product for each series
                     products = map product series
                 -- Find the maximum product.
                 -- The 'products' list will not be empty due to the size > n check above,
                 -- unless n=0 and size=0, which is handled earlier.
                 in Right (maximum products)
