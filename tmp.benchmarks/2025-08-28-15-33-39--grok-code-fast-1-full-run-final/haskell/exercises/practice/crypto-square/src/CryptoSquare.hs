module CryptoSquare (encode) where

import Data.Char (toLower, isAlpha)
import Data.List (transpose)

encode :: String -> String
encode xs =
  let norm = map toLower $ filter isAlpha xs
      len = length norm
      c = findC len
      r = (len + c - 1) `div` c
      padded = norm ++ replicate (r * c - len) ' '
      rows = chunksOf c padded
      cols = transpose rows
  in unwords cols

findC :: Int -> Int
findC len = head [c | c <- [1..], let r = (len + c - 1) `div` c, c >= r, c - r <= 1]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
