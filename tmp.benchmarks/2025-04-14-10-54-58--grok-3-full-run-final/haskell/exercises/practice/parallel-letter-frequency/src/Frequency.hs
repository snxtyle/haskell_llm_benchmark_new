module Frequency (frequency) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Par (parMap, runPar)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = 
    let
        -- Process each text to count frequencies
        countFrequencies :: Text -> Map Char Int
        countFrequencies text = 
            Text.foldl' (\acc c -> Map.insertWith (+) c 1 acc) Map.empty text
        -- Merge frequency maps
        mergeMaps :: [Map Char Int] -> Map Char Int
        mergeMaps = foldl (Map.unionWith (+)) Map.empty
    in
        -- Process texts in parallel and merge results
        runPar $ do
            let chunkSize = max 1 (length texts `div` nWorkers)
            let chunks = chunk chunkSize texts
            results <- parMap (map countFrequencies) chunks
            return $ mergeMaps (concat results)
  where
    chunk _ [] = []
    chunk n xs = let (h, t) = splitAt n xs in h : chunk n t
