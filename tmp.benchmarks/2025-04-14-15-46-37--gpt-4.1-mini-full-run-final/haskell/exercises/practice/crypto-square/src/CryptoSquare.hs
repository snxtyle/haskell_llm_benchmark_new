module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs = unwords $ map padRight columns
  where
    -- Normalize input: remove non-alphanumeric, lowercase
    normalized = map toLower $ filter isAlphaNum xs

    len = length normalized

    -- Determine rows and columns according to rules:
    -- c >= r, c - r <= 1, r * c >= len, c minimal
    (r, c) = findDimensions len

    -- Break normalized text into rows of length c
    rows = chunk c normalized

    -- Transpose rows to get columns
    columns = transpose rows

    -- Pad each column to length r with trailing spaces if needed
    padRight col = col ++ replicate (r - length col) ' '

-- Find rows and columns for the rectangle
findDimensions :: Int -> (Int, Int)
findDimensions len = head
  [ (r, c)
  | r <- [1..len]
  , c <- [r..r+1]
  , r * c >= len
  ]

-- Chunk a list into pieces of given size
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)
