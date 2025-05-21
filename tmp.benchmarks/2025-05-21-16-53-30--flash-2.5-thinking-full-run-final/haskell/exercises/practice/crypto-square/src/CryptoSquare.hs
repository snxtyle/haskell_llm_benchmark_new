module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose, intercalate)

-- Helper to split a list into chunks of a given size.
-- This is a manual implementation of `chunksOf` from Data.List.Split,
-- as Data.List.Split is not part of the standard `base` library.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

encode :: String -> String
encode text =
    let
        -- 1. Normalize the input: remove spaces/punctuation, down-case
        normalizedText = map toLower $ filter isAlphaNum text
        len = length normalizedText

        -- 2. Determine rectangle dimensions (r and c)
        -- Find smallest c such that r * c >= len, c >= r, and c - r <= 1
        -- Calculate c as ceiling(sqrt(len))
        s = sqrt (fromIntegral len :: Double)
        c = ceiling s
        r = floor s
        (finalR, finalC)
            | r * c < len = (c, c) -- If r*c is too small, make it a square of c
            | otherwise   = (r, c)

        -- 3. Pad the normalized string with spaces to fit the rectangle (r * c)
        paddedText = take (finalR * finalC) (normalizedText ++ repeat ' ')

        -- 4. Form the rectangle (rows)
        -- Split the padded string into rows of length finalC
        rows = chunksOf finalC paddedText

        -- 5. Read down columns to encode (transpose the rows)
        columns = transpose rows

        -- 6. Format the output: join column strings with spaces
    in
        intercalate " " columns
