module Frequency (frequency) where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Char (isLetter, toLower)
import Data.Foldable (foldl')
import Data.Map.Strict (Map, unionWith, empty, insertWith)
import Data.Text (Text)
import qualified Data.Text as T

-- | Count the frequency of letters in a single Text value.
--   Ignores case and non-letter characters.
countChars :: Text -> Map Char Int
countChars = T.foldl' countChar empty
  where
    countChar :: Map Char Int -> Char -> Map Char Int
    countChar acc c
      | isLetter c = insertWith (+) (toLower c) 1 acc
      | otherwise  = acc

-- | Calculate the frequency of letters in a list of texts using parallel computation.
--   The `nWorkers` parameter currently doesn't directly control the chunking strategy
--   used by `parMap rpar`, but GHC's runtime system (-N option) will utilize
--   available cores based on `nWorkers`. More sophisticated chunking could be
--   implemented using `parListChunk` if needed.
frequency :: Int -> [Text] -> Map Char Int
frequency _nWorkers texts = mconcat mapsWithCombine
  where
    -- Process each text in parallel to get a list of frequency maps
    maps = parMap rpar countChars texts
    -- Combine the maps using unionWith (+)
    mapsWithCombine = map (unionWith (+)) maps

-- | A helper function to combine a list of maps.
--   `mconcat` with `unionWith (+)` achieves this efficiently for Maps.
--   Note: `Data.Map.Strict.unionsWith (+)` is another way to achieve this.
mconcat :: [Map Char Int] -> Map Char Int
mconcat = foldl' (unionWith (+)) empty

