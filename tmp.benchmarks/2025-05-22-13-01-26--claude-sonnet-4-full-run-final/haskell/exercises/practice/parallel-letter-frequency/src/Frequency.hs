module Frequency (frequency) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isLetter, toLower)
import Control.Parallel.Strategies

-- | Count frequency of letters in texts using parallel computation
frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = 
    let chunks = chunkList nWorkers texts
        countMaps = map countLettersInTexts chunks `using` parList rdeepseq
    in Map.unionsWith (+) countMaps

-- | Split a list into n roughly equal chunks
chunkList :: Int -> [a] -> [[a]]
chunkList n xs
    | n <= 0 = [xs]
    | otherwise = 
        let len = length xs
            chunkSize = max 1 (len `div` n)
            remainder = len `mod` n
        in go chunkSize remainder xs
  where
    go _ _ [] = []
    go size rem items =
        let currentChunkSize = if rem > 0 then size + 1 else size
            (chunk, rest) = splitAt currentChunkSize items
        in chunk : go size (max 0 (rem - 1)) rest

-- | Count letters in a list of texts
countLettersInTexts :: [Text] -> Map Char Int
countLettersInTexts texts = 
    let allChars = concatMap T.unpack texts
        letters = filter isLetter allChars
        lowerLetters = map toLower letters
    in Map.fromListWith (+) [(c, 1) | c <- lowerLetters]
