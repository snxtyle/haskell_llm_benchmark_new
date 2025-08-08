module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (intercalate, transpose)

encode :: String -> String
encode xs =
  let s = normalize xs
      n = length s
  in if n == 0
       then ""
       else
         let c = ceiling (sqrt (fromIntegral n :: Double))
             rows = map (padRight c) (chunks c s)
             cols = transpose rows
         in intercalate " " cols

normalize :: String -> String
normalize = map toLower . filter isAlphaNum

padRight :: Int -> String -> String
padRight k str = str ++ replicate (k - length str) ' '

chunks :: Int -> [a] -> [[a]]
chunks k ys
  | k <= 0 = []
  | null ys = []
  | otherwise = let (h, t) = splitAt k ys in h : chunks k t
