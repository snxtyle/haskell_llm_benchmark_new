module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (intercalate, transpose)

-- | Encode a piece of text using the classic square code.
--
-- The steps are:
--   1. Normalize the input: keep only alphanumeric characters and
--      convert everything to lowercase.
--   2. Work out the rectangle dimensions (rows r and columns c) that satisfy:
--        r * c >= n
--        c >= r
--        c - r <= 1
--   3. Write the normalized text into the rectangle row-wise,
--      padding with spaces on the right to fill the rectangle.
--   4. Read the rectangle column-wise (top to bottom, left to right)
--      producing c chunks, each of length r, separated by spaces.
--
-- >>> encode "If man was meant to stay on the ground, god would have given us roots."
-- "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau "
encode :: String -> String
encode xs
  | null normalized = ""
  | otherwise       = intercalate " " encodedColumns
  where
    -- Step 1: normalization
    normalized = map toLower . filter isAlphaNum $ xs
    n          = length normalized

    -- Step 2: determine rectangle dimensions
    c :: Int
    c = ceiling (sqrt (fromIntegral n :: Double))

    r :: Int
    r = (n + c - 1) `div` c  -- ceiling (n / c), guarantees r <= c and c - r <= 1

    -- Step 3: create rows, padding with spaces to fill the rectangle
    padded      = normalized ++ replicate (r * c - n) ' '
    rows        = chunksOf c padded

    -- Step 4: read column-wise and join with spaces
    encodedColumns = transpose rows

-- | Split a list into chunks of exactly @k@ elements.
--   The last chunk will be shorter only if @k == 0@ (which we never do here).
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)
