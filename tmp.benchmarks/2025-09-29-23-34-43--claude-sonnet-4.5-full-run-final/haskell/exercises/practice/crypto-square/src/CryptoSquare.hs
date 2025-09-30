module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs
  | null normalized = ""
  | otherwise = unwords $ map (padTo r) columns
  where
    normalized = normalize xs
    len = length normalized
    c = columns' len
    r = rows' len c
    rectangle = makeRectangle normalized c
    columns = transpose rectangle
    
    padTo n str = str ++ replicate (n - length str) ' '

-- Normalize: remove non-alphanumeric, convert to lowercase
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Calculate number of columns
columns' :: Int -> Int
columns' len = ceiling (sqrt (fromIntegral len :: Double))

-- Calculate number of rows
rows' :: Int -> Int -> Int
rows' len c
  | c * (c - 1) >= len = c - 1
  | otherwise = c

-- Break normalized text into rows of length c
makeRectangle :: String -> Int -> [String]
makeRectangle [] _ = []
makeRectangle str c = take c str : makeRectangle (drop c str) c
