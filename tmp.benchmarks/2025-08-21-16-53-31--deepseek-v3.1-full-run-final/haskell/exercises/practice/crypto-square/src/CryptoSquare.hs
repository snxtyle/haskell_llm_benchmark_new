module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs = 
    let normalized = [toLower char | char <- xs, isAlphaNum char]
        len = length normalized
        (cols, rows) = findDimensions len
        chunks = chunkify cols normalized
        padded = padChunks cols rows chunks
        transposed = transpose padded
        encodedChunks = map (take rows) transposed
    in unwords encodedChunks

findDimensions :: Int -> (Int, Int)
findDimensions len
    | len == 0 = (0, 0)
    | otherwise =
        let c = ceiling (sqrt (fromIntegral len :: Double))
            r = if c * (c - 1) >= len then c - 1 else c
        in (c, r)

chunkify :: Int -> String -> [String]
chunkify _ [] = []
chunkify n xs = take n xs : chunkify n (drop n xs)

padChunks :: Int -> Int -> [String] -> [String]
padChunks cols rows chunks = 
    take cols $ map pad chunks
    where
        pad chunk = chunk ++ replicate (rows - length chunk) ' '
