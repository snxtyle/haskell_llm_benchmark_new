module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.Traversable (traverse)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

-- Helper function to convert a character to an Integer if it's a digit.
-- Returns an Either Error Integer to propagate potential InvalidDigit errors.
charToDigit :: Char -> Either Error Integer
charToDigit c
  | isDigit c = Right $ fromIntegral (digitToInt c)
  | otherwise = Left (InvalidDigit c)

-- Helper function to generate all contiguous sublists (series) of a given size `n`
-- from a list `xs`.
generateSeries :: Int -> [a] -> [[a]]
generateSeries n xs
  -- This case should ideally not be hit if called from largestProduct,
  -- as size > 0 is ensured before calling this for series generation.
  | n <= 0    = [] 
  | otherwise = go xs
  where
    go lst
      -- If the remaining list is shorter than the desired series length,
      -- no more series can be formed.
      | length lst < n = []
      -- Otherwise, take the next series and recurse on the tail of the list.
      | otherwise      = take n lst : go (tail lst)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digitsStr
  -- Validate span: size cannot be negative.
  | size < 0             = Left InvalidSpan
  -- Special case: if span is 0, product is 1.
  | size == 0            = Right 1
  -- Validate span: size cannot be greater than the length of the digits string.
  | size > length digitsStr = Left InvalidSpan
  -- If all validations pass so far:
  | otherwise =
      -- Attempt to convert the input string of digits to a list of Integers.
      -- traverse applies charToDigit to each character and collects the results
      -- in an Either context. If any charToDigit call returns Left,
      -- traverse short-circuits and returns that Left value.
      case traverse charToDigit digitsStr of
        Left err -> Left err -- Propagate InvalidDigit error
        Right numbers ->
          -- At this point:
          -- 1. size > 0
          -- 2. size <= length numbers (derived from size <= length digitsStr)
          -- 3. numbers is a list of Integers.
          -- 4. If digitsStr was empty, 'size > length digitsStr' would have been true (since size > 0 here),
          --    so 'numbers' is not empty if digitsStr was not empty.
          
          -- Generate all series of the given size from the list of numbers.
          let allSeries = generateSeries size numbers
          in 
            -- Since size > 0 and size <= length numbers, 'allSeries' will not be empty.
            -- For example, if size == length numbers, allSeries will be '[numbers]'.
            -- Calculate the product of each series, then find the maximum among these products.
            Right $ maximum $ map product allSeries
