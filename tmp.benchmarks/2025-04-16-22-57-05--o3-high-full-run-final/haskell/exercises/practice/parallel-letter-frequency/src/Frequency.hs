module Frequency (frequency) where

import           Control.Parallel            (par, pseq)
import           Data.Char                   (isAlpha, toLower)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T

-- | Count the frequency of each letter in the given texts using the
--   specified number of worker sparks (parallel tasks).
--   Counting is case‑insensitive and any non‑alphabetic characters are ignored.
frequency :: Int       -- ^ number of workers (sparks) to use
          -> [Text]    -- ^ input texts
          -> Map Char Int
frequency nWorkers texts
  | null texts    = Map.empty
  | nWorkers <= 1 = sequentialCounts
  | otherwise     = combineAll partialCounts
  where
    -- Sequential result (used when nWorkers <= 1)
    sequentialCounts = combineAll $ map countText texts

    -- Divide work into (approximately) equal chunks.
    chunkSize = max 1 ((length texts + nWorkers - 1) `div` nWorkers)
    chunks    = chunksOf chunkSize texts

    -- Count every chunk in parallel.
    partialCounts = parMap countChunk chunks

    -- Count all texts belonging to a single chunk.
    countChunk :: [Text] -> Map Char Int
    countChunk = combineAll . map countText

-- | Count the letters in a single piece of text.
countText :: Text -> Map Char Int
countText = T.foldl' step Map.empty
  where
    step acc c
      | isAlpha c = Map.insertWith (+) (toLower c) 1 acc
      | otherwise = acc

-- | Merge many frequency maps together.
combineAll :: [Map Char Int] -> Map Char Int
combineAll = foldr (Map.unionWith (+)) Map.empty

-- | A very small re‑implementation of @parMap@ that only relies on
--   the primitives from "Control.Parallel".
parMap :: (a -> b) -> [a] -> [b]
parMap _ []     = []
parMap f (x:xs) =
  let y   = f x
      ys  = parMap f xs
  in  y `par` (ys `pseq` (y : ys))

-- | Split a list into chunks of (at most) the given size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf k
  | k <= 0    = error "chunksOf: chunk size must be positive"
  | otherwise = go
  where
    go [] = []
    go xs = let (h, t) = splitAt k xs
            in h : go t
