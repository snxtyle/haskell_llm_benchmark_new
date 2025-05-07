module Frequency (frequency) where

import qualified Data.Map.Strict as M
import Data.Map (Map) -- For the type signature Map Char Int
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isLetter, toLower)
import Control.Parallel.Strategies (parMap, rdeepseq)

-- | Process a list of Text values (a chunk) and return a frequency map.
processChunk :: [Text] -> Map Char Int
processChunk textsToProcess = T.foldl' countChar M.empty (T.concat textsToProcess)
  where
    -- | Accumulator function for T.foldl'. Updates the map with character counts.
    countChar :: Map Char Int -> Char -> Map Char Int
    countChar acc c =
      if isLetter c
      then M.insertWith (+) (toLower c) 1 acc
      else acc

-- | Splits a list into a specified number of sublists (chunks).
--   If numChunks is <= 0, it returns a list containing the original list as its only element,
--   or an empty list if the input list is empty.
--   If the input list is empty, it returns an empty list of chunks.
--   The number of chunks will be at most numChunks, or the number of elements in xs if totalLength < numChunks.
chunkInto :: Int -> [a] -> [[a]]
chunkInto numChunks xs
  | numChunks <= 0 = if null xs then [] else [xs] -- Handle non-positive numChunks
  | null xs        = []                           -- Handle empty input list
  | otherwise      =
      let totalLength = length xs
          -- Calculate chunkSize: ceiling(totalLength / numChunks)
          -- This ensures that we get at most numChunks, and each chunk is non-empty if totalLength > 0.
          chunkSize = max 1 ((totalLength + numChunks - 1) `div` numChunks)
      in splitBy chunkSize xs
  where
    splitBy :: Int -> [a] -> [[a]]
    splitBy _ [] = []
    splitBy size list = take size list : splitBy size (drop size list)

-- | Computes the frequency of each character in a list of texts, using parallel computation.
frequency :: Int    -- ^ Number of workers to use for parallel processing.
          -> [Text] -- ^ A list of Text values.
          -> Map Char Int -- ^ A map from characters to their total frequencies.
frequency nWorkers texts
  | nWorkers <= 1 = processChunk texts -- Sequential execution for 1 or fewer workers
  | null texts    = M.empty            -- No texts, no frequencies
  | otherwise     =
      let chunks = chunkInto nWorkers texts
          -- parMap applies processChunk to each chunk in parallel.
          -- rdeepseq strategy ensures that each Map Char Int is fully evaluated in the parallel spark.
          listOfMaps = parMap rdeepseq processChunk chunks
      -- M.unionsWith (+) efficiently merges a list of maps, summing values for common keys.
      in M.unionsWith (+) listOfMaps
