module Frequency (frequency) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isLetter, toLower)
import Control.Parallel.Strategies

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = 
    let -- Split texts into chunks for parallel processing
        chunkSize = max 1 (length texts `div` nWorkers)
        chunks = chunksOf chunkSize texts
        
        -- Process each chunk in parallel
        frequencies = parMap rdeepseq countFrequencies chunks
        
    in -- Combine all frequency maps
       Map.unionsWith (+) frequencies

-- Count letter frequencies in a list of texts
countFrequencies :: [Text] -> Map Char Int
countFrequencies texts = Map.unionsWith (+) (map countText texts)

-- Count letter frequencies in a single text
countText :: Text -> Map Char Int
countText text = T.foldl' addChar Map.empty text
  where
    addChar acc c
        | isLetter c = Map.insertWith (+) (toLower c) 1 acc
        | otherwise  = acc

-- Helper function to split a list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
