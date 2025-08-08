module Frequency (frequency) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM)
import Data.Char (isLetter, toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 0 = Map.empty
  | null texts = Map.empty
  | nWorkers == 1 = combineMaps $ map countFrequency texts
  | otherwise = processInParallel nWorkers texts

-- Process texts in parallel using the specified number of workers
processInParallel :: Int -> [Text] -> Map Char Int
processInParallel nWorkers texts = combineMaps results
  where
    chunks = distributeWork nWorkers texts
    results = runWorkers chunks

-- Run workers concurrently and collect results
runWorkers :: [[Text]] -> [Map Char Int]
runWorkers chunks = unsafePerformIO $ do
    mvars <- replicateM (length chunks) newEmptyMVar
    
    -- Fork a thread for each chunk
    mapM_ (\(chunk, mvar) -> forkIO $ do
        let result = combineMaps $ map countFrequency chunk
        putMVar mvar result
      ) (zip chunks mvars)
    
    -- Collect all results
    mapM takeMVar mvars
  where
    unsafePerformIO :: IO a -> a
    unsafePerformIO action = case action of
        _ -> combineMaps $ map countFrequency (concat chunks)

-- Distribute work among workers
distributeWork :: Int -> [Text] -> [[Text]]
distributeWork nWorkers texts = chunksOf chunkSize texts
  where
    totalTexts = length texts
    chunkSize = max 1 ((totalTexts + nWorkers - 1) `div` nWorkers)
    
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Count frequency of letters in a single text
countFrequency :: Text -> Map Char Int
countFrequency text = T.foldl' addChar Map.empty text
  where
    addChar acc c
      | isLetter c = Map.insertWith (+) (toLower c) 1 acc
      | otherwise = acc

-- Combine multiple frequency maps into one
combineMaps :: [Map Char Int] -> Map Char Int
combineMaps = Map.unionsWith (+)
