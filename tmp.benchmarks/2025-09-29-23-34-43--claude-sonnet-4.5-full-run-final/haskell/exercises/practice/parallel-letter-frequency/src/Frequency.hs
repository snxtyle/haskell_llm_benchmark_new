module Frequency (frequency) where

import Data.Char (isAlpha, toLower)
import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel (par, pseq)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 0 = countFrequency texts
  | otherwise = 
      let chunks = splitIntoChunks nWorkers texts
          freqMaps = parMapChunk countFrequency chunks
      in Map.unionsWith (+) freqMaps

-- Parallel map using par and pseq
parMapChunk :: (a -> b) -> [a] -> [b]
parMapChunk _ [] = []
parMapChunk f (x:xs) = 
  let fx = f x
      rest = parMapChunk f xs
  in fx `par` (rest `pseq` (fx : rest))

-- Split texts into n chunks for parallel processing
splitIntoChunks :: Int -> [Text] -> [[Text]]
splitIntoChunks n texts
  | n <= 0 = [texts]
  | null texts = []
  | otherwise = 
      let chunkSize = max 1 (length texts `div` n)
          (chunk, rest) = splitAt chunkSize texts
      in chunk : splitIntoChunks (n - 1) rest

-- Count frequency of letters in a list of texts
countFrequency :: [Text] -> Map Char Int
countFrequency texts = 
  let allText = T.concat texts
      letters = filter isAlpha $ T.unpack allText
      lowerLetters = map toLower letters
  in foldr (\c -> Map.insertWith (+) c 1) Map.empty lowerLetters
