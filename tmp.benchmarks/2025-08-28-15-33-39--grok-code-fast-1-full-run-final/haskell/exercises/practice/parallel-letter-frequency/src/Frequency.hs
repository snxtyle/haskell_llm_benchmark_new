module Frequency (frequency) where

import Data.Map (Map, empty, insertWith, unionWith)
import Data.Text (Text, foldl')
import Data.Char (isLetter, toLower)
import Control.Parallel.Strategies (parMap, rdeepseq)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 0 = frequency 1 texts
  | otherwise = foldl' (unionWith (+)) empty freqs
  where
    chunks = splitInto nWorkers texts
    freqs = parMap rdeepseq countFreq chunks
    countFreq ts = foldl' (unionWith (+)) empty (map countSingle ts)
    countSingle t = foldl' (\m c -> if isLetter c then insertWith (+) (toLower c) 1 m else m) empty t

splitInto :: Int -> [a] -> [[a]]
splitInto n xs
  | n <= 1 = [xs]
  | otherwise = let len = length xs `div` n
                    (a, b) = splitAt len xs
                in a : splitInto (n-1) b
