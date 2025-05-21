module Series (Error(..), largestProduct) where

import Data.List (tails)
import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

-- Helper function to convert a character to an Integer,
-- returning an Error if it's not a digit.
charToInteger :: Char -> Either Error Integer
charToInteger c
  | isDigit c = Right (fromIntegral (digitToInt c))
  | otherwise = Left (InvalidDigit c)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  -- Handle invalid span: negative size
  | size < 0 = Left InvalidSpan
  -- Handle invalid span: size greater than the length of the digits string
  | size > length digits = Left InvalidSpan
  -- Handle the special case where size is 0.
  -- The product of an empty series is conventionally 1.
  | size == 0 = Right 1
  | otherwise = do
      -- First, convert the string of digits to a list of Integers.
      -- The 'traverse' function will propagate any 'InvalidDigit' errors.
      nums <- traverse charToInteger digits
      
      -- Generate all possible series (sublists) of the specified 'size'.
      -- 'tails' creates all suffixes of the list.
      -- We then take 'size' elements from each suffix that is long enough.
      let seriesList = [take size s | s <- tails nums, length s >= size]
      
      -- Calculate the product for each series.
      let products = map product seriesList
      
      -- Find the maximum product among all series.
      -- 'maximum' is safe here because if we reach this point,
      -- 'size' is positive and less than or equal to 'length digits',
      -- ensuring 'seriesList' will not be empty.
      return (maximum products)
