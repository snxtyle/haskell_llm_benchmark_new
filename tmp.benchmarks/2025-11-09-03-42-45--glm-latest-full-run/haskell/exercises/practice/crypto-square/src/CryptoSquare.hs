module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

-- | Breaks a list into chunks of a given size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Encodes a string using the square code method.
encode :: String -> String
encode xs
  -- If the normalized string is empty, the result is an empty string.
  | null normalized = ""
  | otherwise = unwords $ transpose rectangle
  where
    -- 1. Normalize the input: remove non-alphanumeric chars and lowercase.
    normalized :: String
    normalized = map toLower $ filter isAlphaNum xs

    len :: Int
    len = length normalized

    -- 2. Calculate the dimensions of the rectangle (r x c).
    -- The rectangle should be as close to a square as possible.
    -- c is the number of columns, r is the number of rows.
    -- c is the ceiling of the square root of the length.
    -- r is the ceiling of the length divided by c.
    c :: Int
    c = ceiling $ sqrt $ fromIntegral len

    r :: Int
    r = ceiling $ fromIntegral len / fromIntegral c

    -- 3. Pad the normalized string with spaces to fit the r x c rectangle.
    padded :: String
    padded = normalized ++ replicate (r * c - len) ' '

    -- 4. Form the rectangle by chunking the padded string into rows of length c.
    rectangle :: [String]
    rectangle = chunksOf c padded
