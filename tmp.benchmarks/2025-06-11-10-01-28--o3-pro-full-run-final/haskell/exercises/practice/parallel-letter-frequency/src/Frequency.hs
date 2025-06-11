module Frequency (frequency) where

import GHC.Conc        (par, pseq)          -- par / pseq live in base (GHC.Conc)
import Data.Char       (isLetter, toLower)
import Data.List       (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text       (Text)
import qualified Data.Text as T

-- | Count the frequency of letters (case–insensitive) appearing in a list of
-- texts, using the supplied number of parallel workers.
--
-- The result contains a mapping from each letter (always lower-cased) to the
-- number of times it occurs in all input texts.
--
--   • nWorkers <= 1  -> computation is performed sequentially.
--   • nWorkers  > 1  -> the work is split into @nWorkers@ chunks that are
--                       evaluated in parallel using GHC sparks (`par`).
frequency :: Int      -- ^ Number of workers to employ
          -> [Text]   -- ^ Input texts
          -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 1 = sequentialFreq texts
  | otherwise     = parallelFreq (max 1 nWorkers) texts

-- Sequential reference implementation ---------------------------------------

sequentialFreq :: [Text] -> Map Char Int
sequentialFreq = foldl' (Map.unionWith (+)) Map.empty . map letterFrequencies

-- Parallel implementation ----------------------------------------------------

parallelFreq :: Int -> [Text] -> Map Char Int
parallelFreq nWorkers ts =
    -- Compute each chunk’s frequency map in parallel
    let chunkMaps = parMap' chunkFreq (chunk nWorkers ts)
    in foldl' (Map.unionWith (+)) Map.empty chunkMaps
  where
    -- Frequency map for a chunk (list) of texts
    chunkFreq :: [Text] -> Map Char Int
    chunkFreq = foldl' (Map.unionWith (+)) Map.empty . map letterFrequencies

-- Utilities ------------------------------------------------------------------

-- | Compute the frequencies of letters inside a single text.
letterFrequencies :: Text -> Map Char Int
letterFrequencies =
    T.foldl' step Map.empty
  where
    step acc ch
      | isLetter ch = Map.insertWith (+) (toLower ch) 1 acc
      | otherwise   = acc

-- | Split a list into at most @n@ contiguous chunks.
chunk :: Int -> [a] -> [[a]]
chunk n xs
  | n <= 1    = [xs]
  | null xs   = replicate n []
  | otherwise = go xs
  where
    len  = length xs
    size = max 1 $ (len + n - 1) `div` n  -- ceiling division
    go [] = []
    go ys = let (front, rest) = splitAt size ys
            in front : go rest

-- | A very small ‘parMap’ that creates one spark per list element and ensures
--   each element is fully evaluated.
parMap' :: NF a => (t -> a) -> [t] -> [a]
parMap' _ []     = []
parMap' f (x:xs) =
    let r  = f x
        rs = parMap' f xs
    in force r `par` (rs `pseq` (r : rs))

-- | Minimal replacement for `deepseq`’s NFData, specialised for our needs:
--   we only have to ensure the value is evaluated enough to avoid retaining
--   thunks when combining the results.
class NF a where
    force :: a -> ()

instance NF (Map k v) where
    force m = Map.size m `seq` ()

instance NF a => NF [a] where
    force xs = foldl' (\z x -> force x `seq` z) () xs
