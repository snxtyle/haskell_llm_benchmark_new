module Frequency (frequency) where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Char (isLetter, toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
  let chunks = splitInto nWorkers texts
      maps = parMap rdeepseq processChunk chunks
   in M.unionsWith (+) maps
  where
    processChunk :: [Text] -> Map Char Int
    processChunk = M.unionsWith (+) . map countChars

    countChars :: Text -> Map Char Int
    countChars = T.foldl' countChar M.empty
      where
        countChar acc c =
          if isLetter c
            then M.insertWith (+) (toLower c) 1 acc
            else acc

    splitInto :: Int -> [a] -> [[a]]
    splitInto _ [] = []
    splitInto n xs
      | n > 1 =
        let len = length xs
            chunkSize = (len + n - 1) `div` n
            (chunk, rest) = splitAt chunkSize xs
         in chunk : splitInto (n - 1) rest
      | otherwise = [xs]
