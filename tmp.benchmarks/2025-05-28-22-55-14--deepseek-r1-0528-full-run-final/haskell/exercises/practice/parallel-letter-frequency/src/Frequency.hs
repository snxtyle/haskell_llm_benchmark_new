module Frequency (frequency) where

import Control.Parallel.Strategies
import Data.Map (Map, empty, insertWith, unionsWith)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower, isLetter)
import Data.List (foldl')

frequency :: Int -> [Text] -> Map Char Int
frequency _ [] = empty
frequency nWorkers texts = 
  let numTexts = length texts
      -- Handle invalid worker count
      nChunks = max 1 . min (min nWorkers numTexts) $ numTexts
      chunkSize = max 1 (numTexts `div` nChunks)
      chunks = chunksOf chunkSize texts
      results = parMap rseq processChunk chunks
  in unionsWith (+) results
  where
    chunksOf n [] = []
    chunksOf n xs = first : chunksOf n rest
      where (first, rest) = splitAt n xs
    processChunk = foldl' (flip countText) empty
    countText :: Text -> Map Char Int -> Map Char Int
    countText t acc = T.foldl' updateMap acc t
      where 
        updateMap ac c 
          | isLetter l = insertWith (+) l 1 ac
          | otherwise  = ac
          where l = toLower c
