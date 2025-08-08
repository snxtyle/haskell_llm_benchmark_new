module Frequency (frequency) where

import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Char (isAlpha)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

-- Count frequency of letters across multiple texts using parallel computation.
-- nWorkers controls the number of parallel tasks used.
frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
  let workers = max 1 nWorkers

      -- Merge a list of frequency maps
      merge :: [Map Char Int] -> Map Char Int
      merge = foldl' (M.unionWith (+)) M.empty

      -- Count letters in a single Text (case-insensitive, letters only)
      countOne :: Text -> Map Char Int
      countOne =
        T.foldl' step M.empty . T.toLower
        where
          step acc c
            | isAlpha c = M.insertWith (+) c 1 acc
            | otherwise = acc

      -- Split the work into approximately even chunks for the given number of workers
      chunks :: [[Text]]
      chunks = splitInto workers texts

      -- Compute partial results per worker
      partials :: [Map Char Int]
      partials = map (merge . map countOne) chunks
  in
      if null texts || workers <= 1
        then merge (map countOne texts)
        else merge (partials `using` parList rdeepseq)

-- Split a list into n parts with sizes as even as possible.
splitInto :: Int -> [a] -> [[a]]
splitInto n xs
  | n <= 1 = [xs]
  | otherwise =
      let len = length xs
          (q, r) = len `quotRem` n
          sizes = replicate r (q + 1) ++ replicate (n - r) q
      in splitBy sizes xs
  where
    splitBy [] _ = []
    splitBy (s:ss) ys =
      let (a, b) = splitAt s ys
      in a : splitBy ss b
