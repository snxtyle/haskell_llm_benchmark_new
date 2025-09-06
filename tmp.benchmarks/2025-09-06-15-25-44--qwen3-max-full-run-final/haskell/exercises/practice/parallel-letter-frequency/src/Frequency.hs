module Frequency (frequency) where

import Data.Map  (Map, fromListWith, unionsWith)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel.Strategies
import Data.Char (toLower)
import Data.List (partition)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = unionsWith (+) $ map (countLetters . T.unpack) texts `using` parListChunk chunkSize rdeepseq
  where
    chunkSize = max 1 (length texts `div` nWorkers)

countLetters :: String -> Map Char Int
countLetters = fromListWith (+) . map (\c -> (c, 1)) . filter isLetter
  where
    isLetter c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
