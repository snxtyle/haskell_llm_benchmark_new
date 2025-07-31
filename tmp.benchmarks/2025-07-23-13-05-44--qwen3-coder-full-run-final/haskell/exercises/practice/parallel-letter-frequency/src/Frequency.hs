module Frequency (frequency) where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Char (toLower, isLetter)
import Control.Parallel (par, pseq)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = 
    let 
        -- Convert all texts to lowercase letters only
        allLetters = [toLower c | t <- texts, c <- Text.unpack t, isLetter c]
        
        -- Split work among workers
        chunks = chunkList (length allLetters `div` nWorkers + 1) allLetters
        
        -- Process each chunk and count frequencies
        freqMaps = map countFreqs chunks
        
        -- Combine frequency maps using parallel computation
        combinedFreq = combineFreqMaps freqMaps
    in
        combinedFreq
  where
    countFreqs :: String -> Map Char Int
    countFreqs = foldr (\c acc -> Map.insertWith (+) c 1 acc) Map.empty
    
    chunkList :: Int -> [a] -> [[a]]
    chunkList _ [] = []
    chunkList n xs = take n xs : chunkList n (drop n xs)
    
    combineFreqMaps :: [Map Char Int] -> Map Char Int
    combineFreqMaps [] = Map.empty
    combineFreqMaps [m] = m
    combineFreqMaps (m:ms) = 
        let rest = combineFreqMaps ms
        in m `par` (rest `pseq` Map.unionWith (+) m rest)
