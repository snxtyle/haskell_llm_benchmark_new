module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs = unwords $ map padToLength chunks
  where
    normalized = normalize xs
    len = length normalized
    (c, r) = calculateDimensions len
    paddedNormalized = normalized ++ replicate (r * c - len) ' '
    rows = chunksOf c paddedNormalized
    columns = transpose rows
    chunks = columns
    padToLength = take r . (++ repeat ' ')

-- Normalize: keep only alphanumeric characters and convert to lowercase
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Calculate dimensions: find c and r such that:
-- r * c >= length, c >= r, and c - r <= 1
calculateDimensions :: Int -> (Int, Int)
calculateDimensions n
  | n == 0 = (0, 0)
  | otherwise = head [(c, r) | c <- [1..n], let r = ceiling (fromIntegral n / fromIntegral c), c >= r, c - r <= 1]

-- Split a list into chunks of size n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
