module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

-- Normalize the input: remove non-alphanum and downcase
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Split a list into chunks of n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

-- Compute the rectangle dimensions (r, c)
rectangleDims :: Int -> (Int, Int)
rectangleDims len = head [(r, c) | c <- [1..], let r = ceiling (fromIntegral len / fromIntegral c), c >= r, c - r <= 1, r * c >= len]

-- Pad a string to length n with spaces
padTo :: Int -> String -> String
padTo n s = s ++ replicate (n - length s) ' '

encode :: String -> String
encode xs
  | null norm = ""
  | otherwise = unwords paddedColumns
  where
    norm = normalize xs
    len = length norm
    (r, c) = rectangleDims len
    rows = chunksOf c norm
    paddedRows = map (padTo c) rows
    columns = transpose paddedRows
    paddedColumns = map (padTo r) columns
