module Say (inEnglish) where

import Data.List (intercalate, reverse)
import Data.Maybe (catMaybes)

-- Constants for number words
ones :: [String]
ones = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

teens :: [String]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

scales :: [String]
scales = ["", "thousand", "million", "billion", "trillion"] -- Up to trillion for 999,999,999,999

-- Helper to convert numbers 0-99 to English
sayLessThanHundred :: Integer -> Maybe String
sayLessThanHundred n
    | n < 0 || n >= 100 = Nothing
    | n < 10 = Just (ones !! fromIntegral n)
    | n < 20 = Just (teens !! fromIntegral (n - 10))
    | otherwise =
        let
            tensDigit = n `div` 10
            onesDigit = n `mod` 10
            tensWord = tens !! fromIntegral tensDigit
            onesWord = if onesDigit == 0 then "" else "-" ++ (ones !! fromIntegral onesDigit)
        in
            Just (tensWord ++ onesWord)

-- Helper to convert numbers 0-999 to English
sayLessThanThousand :: Integer -> Maybe String
sayLessThanThousand n
    | n < 0 || n >= 1000 = Nothing
    | n < 100 = sayLessThanHundred n
    | otherwise =
        let
            hundredsDigit = n `div` 100
            remainder = n `mod` 100
            hundredsPart = (ones !! fromIntegral hundredsDigit) ++ " hundred"
            remainderPart = case sayLessThanHundred remainder of
                                Just r -> if remainder == 0 then "" else " " ++ r
                                Nothing -> "" -- Should not happen for 0-99
        in
            Just (hundredsPart ++ remainderPart)

-- Helper to chunk a number into groups of three digits (from right to left)
chunkNumber :: Integer -> [Integer]
chunkNumber 0 = []
chunkNumber n = (n `mod` 1000) : chunkNumber (n `div` 1000)

-- Main function to convert an Integer to its English word representation
inEnglish :: Integer -> Maybe String
inEnglish n
    -- Validate input range: 0 to 999,999,999,999
    | n < 0 || n > 999999999999 = Nothing
    | n == 0 = Just "zero"
    | otherwise =
        let
            -- Chunks are generated from right to left (least significant to most significant)
            -- e.g., 12345 -> [345, 12]
            chunks = chunkNumber n

            -- Process each chunk and its corresponding scale.
            -- `zipWith` applies a function to elements from two lists.
            -- `catMaybes` filters out `Nothing` values from the list of `Maybe String`.
            parts = catMaybes $ zipWith (\chunk scale ->
                if chunk == 0
                    then Nothing -- Don't include "zero thousand", "zero million", etc.
                    else do
                        chunkWord <- sayLessThanThousand chunk
                        if null scale
                            then Just chunkWord
                            else Just (chunkWord ++ " " ++ scale)
                ) chunks scales

            -- The 'parts' list is generated from right to left (smallest scale to largest scale).
            -- For example, 1234567890 would result in parts like:
            -- ["eight hundred ninety", "five hundred sixty-seven thousand", "two hundred thirty-four million", "one billion"]
            -- We need to reverse this list to get the correct order for joining.
        in
            Just (intercalate " " (Data.List.reverse parts))
