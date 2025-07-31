module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)

encode :: String -> String
encode xs = unwords $ map (take rows) $ transpose paddedRows
  where
    normalized = normalize xs
    len = length normalized
    (rows, cols) = dimensions len
    filledText = normalized ++ replicate (rows * cols - len) ' '
    chunked = chunksOf cols filledText
    paddedRows = map (take cols . (++ repeat ' ')) chunked

normalize :: String -> String
normalize = map toLower . filter isAlphaNum

dimensions :: Int -> (Int, Int)
dimensions 0 = (0, 0)
dimensions n = head [(r, c) | c <- [ ceiling . sqrt . fromIntegral $ n .. ]
                            , let r = ceiling (fromIntegral n / fromIntegral c)
                            , r * c >= n
                            , c >= r
                            , c - r <= 1 ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)
