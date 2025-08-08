module Frequency (frequency) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isLetter, toLower)
import Data.List (foldl')
import Control.Parallel (par)

-- Count the frequency of letters (case-insensitive) across the given texts,
-- using up to nWorkers chunks evaluated in parallel.
frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
  let chunks   = splitInto (max 1 nWorkers) texts
      partials = map countChunk chunks
      -- Combine results strictly
      combined = foldl' (M.unionWith (+)) M.empty partials
      -- Spark evaluation of each partial result in parallel
  in foldr (\m r -> forceMap m `par` r) combined partials

-- Count letters for a chunk (list of Texts)
countChunk :: [Text] -> Map Char Int
countChunk = foldl' (\acc t -> M.unionWith (+) acc (countText t)) M.empty

-- Count letters in a single Text (letters only, case-insensitive)
countText :: Text -> Map Char Int
countText = T.foldl' step M.empty
  where
    step m c
      | isLetter c = M.insertWith' (+) (toLower c) 1 m
      | otherwise  = m

-- Force evaluation of the Map's values enough to make 'par' useful
forceMap :: Map Char Int -> Map Char Int
forceMap m = M.foldl' (+) 0 m `seq` m

-- Split a list into up to k balanced chunks (contiguous partitions).
splitInto :: Int -> [a] -> [[a]]
splitInto k xs
  | k <= 1    = [xs]
  | null xs   = []
  | otherwise =
      let m  = length xs
          k' = min k m
          q  = m `div` k'
          r  = m `mod` k'
      in splitSizes (replicate r (q + 1) ++ replicate (k' - r) q) xs

splitSizes :: [Int] -> [a] -> [[a]]
splitSizes [] _ = []
splitSizes (n:ns) ys =
  let (h, t) = splitAt n ys
  in if null h then [] else h : splitSizes ns t
