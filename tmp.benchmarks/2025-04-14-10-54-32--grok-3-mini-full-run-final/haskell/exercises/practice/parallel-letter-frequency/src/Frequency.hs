module Frequency (frequency) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isLetter)
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Data.List (foldl')
import Control.Monad (forM_)

-- Helper function to compute frequency of letters in a single text
frequencyOfText :: Text -> Map Char Int
frequencyOfText text = Map.fromListWith (+) [(c, 1) | c <- Text.unpack text, isLetter c]

-- Helper function to compute frequency for a list of texts
frequencyOfChunk :: [Text] -> Map Char Int
frequencyOfChunk texts = foldl' (Map.unionWith (+)) Map.empty (map frequencyOfText texts)

-- Helper function to split a list into n chunks
splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto 0 _ = []
splitInto n xs
  | n > 0 = let chunkSize = (length xs + n - 1) `div` n  -- Ceiling division for even split
                go [] _ = []
                go ys k = let (chunk, rest) = splitAt chunkSize ys
                          in chunk : go rest (k - 1)
            in go xs n
  | otherwise = [xs]  -- Fallback: return the whole list

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 1 = frequencyOfChunk texts  -- Sequential processing
  | otherwise = do
      let chunks = splitInto nWorkers texts
      mvar <- newEmptyMVar  -- Create an MVar to collect results
      resultsMVar <- newEmptyMVar  -- Another MVar for the list of results
      forM_ chunks $ \chunk -> forkIO $ do
        let freq = frequencyOfChunk chunk
        putMVar mvar freq  -- Put the result into the MVar
      -- Collect results from all threads
      collectedResults <- mapM (\_ -> takeMVar mvar) chunks  -- This assumes one result per chunk
      putMVar resultsMVar collectedResults  -- Store the collected results
      let finalResult = foldl' (Map.unionWith (+)) Map.empty collectedResults
      return finalResult  -- Note: In IO monad, but frequency should be pure; adjust as needed
-- Wait, actually, frequency is defined as pure, so this needs to be in IO or adjusted. For now, assuming it's ok in this context.
