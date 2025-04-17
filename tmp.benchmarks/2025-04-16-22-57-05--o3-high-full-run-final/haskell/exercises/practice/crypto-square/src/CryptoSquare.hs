module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (intercalate)

-- | Encode a piece of text using the “crypto‑square” cipher.
encode :: String -> String
encode input
  | null normalized = ""
  | otherwise       = intercalate " " columns
  where
    normalized           = normalize input
    (rowsCnt, colsCnt)   = rectangleDimensions (length normalized)
    rows                 = buildRows rowsCnt colsCnt normalized
    columns              = [ [ rows !! r !! c | r <- [0 .. rowsCnt - 1] ]
                           | c <- [0 .. colsCnt - 1]
                           ]

-- Remove all non‑alphanumeric characters and lower‑case the rest.
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Determine the rectangle dimensions (rows, columns) that satisfy:
--   rows * columns >= n
--   columns >= rows
--   columns - rows <= 1
rectangleDimensions :: Int -> (Int, Int)
rectangleDimensions 0 = (0, 0)
rectangleDimensions n
  | root * root == n       = (root, root)
  | root * (root + 1) >= n = (root, root + 1)
  | otherwise              = (root + 1, root + 1)
  where
    root = floor (sqrt (fromIntegral n :: Double))

-- Break the text into rows of the given width, padding with spaces so
-- that the resulting list has exactly the requested number of rows and
-- every row has exactly the requested width.
buildRows :: Int -> Int -> String -> [String]
buildRows rowsNeeded width txt = take rowsNeeded allRows
  where
    rawRows     = chunk width txt
    padRow r    = r ++ replicate (width - length r) ' '
    paddedRows  = map padRow rawRows
    blankRow    = replicate width ' '
    allRows     = paddedRows ++ replicate (rowsNeeded - length paddedRows) blankRow

-- Split a list into successive chunks of the given size.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)
