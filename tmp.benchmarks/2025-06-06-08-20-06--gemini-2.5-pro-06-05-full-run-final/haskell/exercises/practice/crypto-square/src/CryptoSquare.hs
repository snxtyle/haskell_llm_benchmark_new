module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (intercalate, transpose)

encode :: String -> String
encode xs
  | null normalized = ""
  | otherwise =
    let
      len    = length normalized
      c      = ceiling . sqrt . fromIntegral $ len
      r      = ceiling (fromIntegral len / fromIntegral c)
      padded = normalized ++ replicate (r * c - len) ' '

      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n ys = take n ys : chunksOf n (drop n ys)

    in intercalate " " . transpose . chunksOf c $ padded
  where
    normalized = filter isAlphaNum $ map toLower xs
