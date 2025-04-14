module Frequency (frequency) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Char (isLetter)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- Count frequency of letters in a single Text
countFreq :: Text -> Map Char Int
countFreq txt = T.foldl' go Map.empty txt
  where
    go m c
      | isLetter c = Map.insertWith (+) c 1 m
      | otherwise  = m

-- Merge two frequency maps
mergeFreq :: Map Char Int -> Map Char Int -> Map Char Int
mergeFreq = Map.unionWith (+)

-- Split list into n chunks as evenly as possible
splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks n xs = go xs sizes
  where
    len = length xs
    (q, r) = len `quotRem` n
    sizes = replicate r (q + 1) ++ replicate (n - r) q

    go [] _ = []
    go ys (sz : szs) = let (chunk, rest) = splitAt sz ys in chunk : go rest szs
    go _ [] = []

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 0 = error "Number of workers must be positive"
  | null texts = Map.empty
  | otherwise = 
      let chunks = splitIntoChunks nWorkers texts
          -- For each chunk, count frequencies of all texts in it combined
          countChunk :: [Text] -> Map Char Int
          countChunk ts = foldl mergeFreq Map.empty (map countFreq ts)
      in
        -- Run counting in parallel on chunks
        let results = mapConcurrently countChunk chunks
        in
          -- Wait for results and merge all
          unsafePerformIO $ do
            res <- results
            return $ Map.unionsWith (+) res

import System.IO.Unsafe (unsafePerformIO)
