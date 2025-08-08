module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs 
  | null normalized = ""
  | otherwise = unwords $ transpose rectangle
  where
    -- Step 1: Normalize the input
    normalized = map toLower $ filter isAlphaNum xs
    
    -- Step 2: Calculate rectangle dimensions
    len = length normalized
    cols = ceiling $ sqrt $ fromIntegral len
    rows = ceiling $ fromIntegral len / fromIntegral cols
    
    -- Step 3: Create rectangle with padding
    padded = normalized ++ replicate (cols * rows - len) ' '
    rectangle = chunks cols padded
    
    -- Helper function to split a list into chunks of size n
    chunks :: Int -> [a] -> [[a]]
    chunks _ [] = []
    chunks n list = take n list : chunks n (drop n list)
