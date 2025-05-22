module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs = unwords $ transpose $ padRows $ chunksOf cols normalized
  where
    normalized = map toLower $ filter isAlphaNum xs
    len = length normalized
    cols = ceiling $ sqrt $ fromIntegral len
    rows = ceiling $ fromIntegral len / fromIntegral cols
    
    -- Split a list into chunks of size n
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n list = take n list : chunksOf n (drop n list)
    
    -- Pad rows to ensure all rows have the same length
    padRows :: [[Char]] -> [[Char]]
    padRows chunks = map (padToLength cols) chunks
    
    -- Pad a string to a specific length with spaces
    padToLength :: Int -> String -> String
    padToLength n str = str ++ replicate (n - length str) ' '
    
    -- Calculate ceiling of a floating point number
    ceiling :: Double -> Int
    ceiling x = if fromIntegral (floor x :: Int) == x 
                then floor x 
                else floor x + 1
