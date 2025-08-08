module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs =
  let normalized = normalize xs
  in if null normalized
       then ""
       else
         let n = length normalized
             c = ceiling (sqrt (fromIntegral n :: Double))
             rows = chunksOf c normalized
             paddedRows = map (padRight c) rows
             columns = transpose paddedRows
         in unwords columns

normalize :: String -> String
normalize = map toLower . filter isAlphaNum

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (h, t) = splitAt n xs
  in h : chunksOf n t

padRight :: Int -> String -> String
padRight len s = s ++ replicate (len - length s) ' '
