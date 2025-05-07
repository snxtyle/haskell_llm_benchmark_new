module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

-- Normalization function: remove non-alphanumeric characters and convert to lowercase.
normalize :: String -> String
normalize = filter isAlphaNum . map toLower

-- Integer ceiling division: ceil(a/b).
-- Precondition: b > 0. This is ensured by the caller (calculateRectDimensions).
ceilDiv :: Int -> Int -> Int
ceilDiv a b
    | a == 0    = 0
    -- Standard formula for ceiling division for positive divisor b
    | otherwise = (a + b - 1) `div` b

-- Calculate rectangle dimensions (rows, cols).
-- Given length L of the normalized text, find r (rows) and c (cols) such that:
-- 1. r * c >= L
-- 2. c >= r
-- 3. c - r <= 1
-- The chosen c should be the smallest possible integer satisfying these.
-- These conditions are met by:
-- c = ceil(sqrt(L))
-- r = ceil(L / c) using integer division.
calculateRectDimensions :: Int -> (Int, Int) -- (rows, cols)
calculateRectDimensions len
    | len == 0 = (0, 0) -- No characters, no rectangle.
    | otherwise =
        let c_double = sqrt (fromIntegral len :: Double)
            c = ceiling c_double
            -- c will be >= 1 if len > 0.
            r = ceilDiv len c
        in (r, c)

-- Split a list into chunks of a given size.
-- Precondition: n > 0. This is ensured by the caller (encode function).
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

encode :: String -> String
encode plainText =
    let normalizedText = normalize plainText
        len = length normalizedText
    in if len == 0
        then "" -- Encoding an empty string results in an empty string.
        else
            let -- Determine rectangle dimensions.
                -- r: number of rows, c: number of columns.
                (r, c) = calculateRectDimensions len

                -- Pad the normalized string with spaces to fill the r x c rectangle.
                -- The total number of characters in the rectangle is r * c.
                paddedText = take (r * c) (normalizedText ++ repeat ' ')

                -- Arrange the padded text into rows.
                -- This results in 'r' rows, each containing 'c' characters.
                rectRows = chunksOf c paddedText

                -- Transpose the rectangle to read by columns.
                -- This results in 'c' columns, each containing 'r' characters.
                rectCols = transpose rectRows

                -- Join the columns with spaces to form the final encoded message.
            in unwords rectCols
