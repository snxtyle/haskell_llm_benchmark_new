module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)
import Data.Maybe (listToMaybe)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct spanSize digits
    | spanSize < 0             = Left InvalidSpan
    | spanSize > length digits = Left InvalidSpan
    | spanSize == 0            = Right 1
    | otherwise =
        case firstInvalidDigit digits of
            Just c  -> Left (InvalidDigit c)
            Nothing -> Right $ maximum $ map seriesProduct $ windows spanSize digits
  where
    -- Find the first non‑digit character, if any
    firstInvalidDigit :: String -> Maybe Char
    firstInvalidDigit = listToMaybe . filter (not . isDigit)

    -- All contiguous subsequences (windows) of length n
    windows :: Int -> [a] -> [[a]]
    windows n xs = takeWhile ((== n) . length) $ map (take n) (tails xs)

    -- Product of the digits in a window
    seriesProduct :: String -> Integer
    seriesProduct = product . map (toInteger . digitToInt)
