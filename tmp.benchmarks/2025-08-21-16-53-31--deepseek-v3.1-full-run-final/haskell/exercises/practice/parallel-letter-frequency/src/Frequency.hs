module Frequency (frequency) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isLetter, toLower)
import Control.Parallel (par, pseq)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
    | nWorkers <= 1 = countLettersInChunk texts
    | otherwise = 
        let chunks = splitIntoChunks nWorkers texts
            (firstChunk:restChunks) = chunks
            firstResult = countLettersInChunk firstChunk
            restResults = map countLettersInChunk restChunks
        in firstResult `par` (mergeFrequencies restResults `pseq` mergeFrequencies (firstResult : restResults))

-- Split texts into roughly equal chunks for parallel processing
splitIntoChunks :: Int -> [Text] -> [[Text]]
splitIntoChunks nWorkers texts
    | nWorkers <= 1 = [texts]
    | otherwise = splitIntoNChunks nWorkers texts

splitIntoNChunks :: Int -> [Text] -> [[Text]]
splitIntoNChunks n texts = go n (length texts) texts
  where
    go _ _ [] = []
    go workers remaining texts =
        let chunkSize = (remaining + workers - 1) `div` workers
            (chunk, rest) = splitAt chunkSize texts
        in chunk : go (workers - 1) (remaining - chunkSize) rest

-- Count letters in a chunk of texts
countLettersInChunk :: [Text] -> Map Char Int
countLettersInChunk texts = 
    foldl (\acc text -> T.foldl' countChar acc (T.toLower text)) Map.empty texts
  where
    countChar acc char
        | isLetter char = Map.insertWith (+) char 1 acc
        | otherwise = acc

-- Merge frequency maps from different chunks
mergeFrequencies :: [Map Char Int] -> Map Char Int
mergeFrequencies = foldl (Map.unionWith (+)) Map.empty
