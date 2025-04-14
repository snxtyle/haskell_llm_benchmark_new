module Frequency (frequency) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Char (isLetter, toLower)

-- Count the frequency of letters in a single Text
letterFreq :: Text -> Map Char Int
letterFreq = T.foldl' go Map.empty
  where
    go acc c
      | isLetter c = Map.insertWith (+) (toLower c) 1 acc
      | otherwise  = acc

-- Merge two frequency maps
mergeFreq :: Map Char Int -> Map Char Int -> Map Char Int
mergeFreq = Map.unionWith (+)

-- Split a list into n chunks as evenly as possible
splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks n xs
  | n <= 1    = [xs]
  | otherwise = go xs (length xs `div` n) (length xs `mod` n) n
  where
    go ys _ _ 0 = [ys]
    go ys chunkSize remChunks chunksLeft =
      let thisChunkSize = chunkSize + (if remChunks > 0 then 1 else 0)
          (chunk, rest) = splitAt thisChunkSize ys
      in chunk : go rest chunkSize (remChunks - 1) (chunksLeft - 1)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
    let chunks = splitIntoChunks nWorkers texts
        -- For each chunk, count the frequency in that chunk (sequentially)
        chunkFreqs = map (foldr (mergeFreq . letterFreq) Map.empty) chunks
                      `using` parList rdeepseq
    in foldr mergeFreq Map.empty chunkFreqs
