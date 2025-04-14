module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose, intersperse)

encode :: String -> String
encode xs = 
    if null normalized then ""
    else unwords $ map (take r . (++ repeat ' ')) $ transpose $ chunk r c normalized
  where
    -- Normalize input: convert to lowercase and keep only alphanumeric characters
    normalized = map toLower $ filter isAlphaNum xs
    
    -- Calculate dimensions of the rectangle
    len = length normalized
    c = ceiling $ sqrt $ fromIntegral len  -- columns
    r = ceiling $ fromIntegral len / fromIntegral c  -- rows
    
    -- Split a list into chunks of specified size
    chunk _ _ [] = []
    chunk rows cols str = 
        let (row, rest) = splitAt cols str
        in row : chunk rows cols rest
