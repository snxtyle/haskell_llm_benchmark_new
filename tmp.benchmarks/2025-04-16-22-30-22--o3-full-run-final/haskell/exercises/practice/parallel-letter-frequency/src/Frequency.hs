{-# LANGUAGE BangPatterns #-}
module Frequency (frequency) where

import           Control.Concurrent      (forkIO, newEmptyMVar, putMVar,
                                          takeMVar)
import           Control.Monad           (forM)
import           Data.Char               (isAlpha, toLower)
import           Data.List               (foldl')
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict as M
import           Data.Text               (Text)
import qualified Data.Text        as T
import           System.IO.Unsafe        (unsafePerformIO)

-- | Count how many times each letter (case‑insensitive) occurs in the given
-- texts, using at most the supplied number of worker threads.
--
-- The function is pure from the caller’s perspective; internally it performs
-- the counting in parallel and uses 'unsafePerformIO' to keep the exposed
-- type pure.
frequency :: Int      -- ^ Number of worker threads (≤ 0 ⇒ sequential)
          -> [Text]   -- ^ Texts to analyse
          -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 1 || null texts = sequentialCount texts
  | otherwise                   = unsafePerformIO parallelCount
  where
    -- Perform the work in parallel, returning a fully combined map.
    parallelCount = do
      let chunks = splitInto nChunks texts
      mvars <- forM chunks $ \chunk -> do
        mv <- newEmptyMVar
        _  <- forkIO $ putMVar mv (sequentialCount chunk)
        return mv
      partials <- mapM takeMVar mvars
      return (mergeAll partials)

    -- Number of chunks we will actually create (no more than length texts)
    nChunks = max 1 (min nWorkers (length texts))

-- | Count the letter frequencies of a list of texts sequentially.
sequentialCount :: [Text] -> Map Char Int
sequentialCount = foldl' mergeMaps M.empty . map countText
  where
    countText :: Text -> Map Char Int
    countText = T.foldl' step M.empty
      where
        step m c
          | isAlpha c = let lc = toLower c
                        in M.insertWith (+) lc 1 m
          | otherwise = m

-- | Merge two frequency maps.
mergeMaps :: Map Char Int -> Map Char Int -> Map Char Int
mergeMaps = M.unionWith (+)

-- | Merge a list of maps into one.
mergeAll :: [Map Char Int] -> Map Char Int
mergeAll = foldl' mergeMaps M.empty

-- | Split a list into @n@ chunks.  The chunks are as even as possible and
-- retain the original ordering.
splitInto :: Int -> [a] -> [[a]]
splitInto n xs = go xs sizes
  where
    len   = length xs
    base  = len `div` n
    extra = len `mod` n
    -- first @extra@ chunks are one element larger
    sizes = replicate extra (base + 1) ++ replicate (n - extra) base

    go []     _          = []
    go ys     []         = [ys]        -- should not happen
    go ys (s:ss)
      | s == 0           = go ys ss
      | otherwise        = let (chunk, rest) = splitAt s ys
                           in chunk : go rest ss
