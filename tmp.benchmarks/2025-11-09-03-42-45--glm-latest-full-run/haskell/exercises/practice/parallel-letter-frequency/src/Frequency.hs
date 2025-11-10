module Frequency (frequency) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, zipWithM_)
import Data.Char (isLetter, toLower)
import Data.Foldable (foldMap)
import Data.Map.Strict (Map, empty, insertWith, unionWith)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

-- | Count the frequency of letters in a single Text.
-- This is the sequential part that will be run in parallel on chunks.
-- It counts letters case-insensitively.
countText :: T.Text -> Map Char Int
countText = T.foldl' increment empty
  where
    increment :: Map Char Int -> Char -> Map Char Int
    increment m c
      | isLetter c' = insertWith (+) c' 1 m
      | otherwise   = m
      where
        c' = toLower c

-- | Split a list into a given number of chunks, distributing elements
-- as evenly as possible. The first few chunks will be one element larger
-- if the list does not divide evenly.
chunkList :: Int -> [a] -> [[a]]
chunkList n xs = take n $ go xs baseSize extra
  where
    len = length xs
    baseSize = len `div` n
    extra = len `mod` n

    go :: [a] -> Int -> Int -> [[a]]
    go _ _ 0 = repeat []
    go ys size e =
        let currentSize = size + if e > 0 then 1 else 0
            (chunk, rest) = splitAt currentSize ys
        in chunk : go rest size (e - 1)

-- | Count the frequency of letters in a list of texts using parallel computation.
-- The first argument is the number of workers to use for parallelization.
-- Note: This implementation uses 'unsafePerformIO' to provide a pure interface
-- over a concurrent computation. This is generally unsafe but is used here
-- to adhere to the required function signature without adding new dependencies.
frequency :: Int -> [T.Text] -> Map Char Int
frequency nWorkers texts
  -- If there's only one worker or no texts, just process sequentially.
  -- This avoids the overhead of parallelism for trivial cases.
  | nWorkers <= 1 = foldMap countText texts
  | null texts    = empty
  | otherwise     = unsafePerformIO $ do
      let chunks = chunkList nWorkers texts
      -- Create an MVar for each chunk to hold the result.
      resultVars <- replicateM (length chunks) (newEmptyMVar :: IO (Map Char Int))
      
      -- A function to process a chunk and put its result in the MVar.
      let processChunk :: [T.Text] -> MVar (Map Char Int) -> IO ()
          processChunk chunk var = putMVar var (foldMap countText chunk)
      
      -- Fork a thread for each chunk to compute its frequency map.
      zipWithM_ (forkIO . processChunk) chunks resultVars
      
      -- Wait for all threads to finish and collect the results.
      results <- mapM takeMVar resultVars
      
      -- Combine the intermediate maps into the final result.
      return $ foldl' (unionWith (+)) empty results
