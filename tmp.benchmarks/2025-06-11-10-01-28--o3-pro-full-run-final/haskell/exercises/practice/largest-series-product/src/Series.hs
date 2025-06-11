module Series (Error(..), largestProduct) where

import Data.Char (digitToInt, isDigit)
import Data.List (tails)

-- | Possible error cases for 'largestProduct'.
data Error
    = InvalidSpan            -- ^ The requested span is negative or larger than the input length.
    | InvalidDigit Char      -- ^ The input contains a non-digit character (reported).
    deriving (Show, Eq)

-- | Compute the largest product of all contiguous digit series of a given length.
--
--   Rules:
--   * If the span is negative or greater than the length of the input, return 'InvalidSpan'.
--   * If any character in the input is not a decimal digit, return 'InvalidDigit' with that character.
--   * A span of 0 always yields a product of 1 (even for an empty string).
largestProduct :: Int      -- ^ span (length of the series)
               -> String   -- ^ input string containing decimal digits
               -> Either Error Integer
largestProduct spanSize digits
    | spanSize < 0 || spanSize > length digits = Left InvalidSpan
    | otherwise = do
        ds <- traverse toDigit digits
        if spanSize == 0
            then Right 1
            else Right . maximum . map productSeries $ windows spanSize ds
  where
    -- Convert a single character to its numeric value, validating that it is a digit.
    toDigit :: Char -> Either Error Integer
    toDigit c
        | isDigit c = Right . fromIntegral $ digitToInt c
        | otherwise = Left (InvalidDigit c)

    -- Generate all contiguous windows of length n.
    windows :: Int -> [a] -> [[a]]
    windows n xs = take (length xs - n + 1) (map (take n) (tails xs))

    -- Product of a list of numbers.
    productSeries :: [Integer] -> Integer
    productSeries = product
