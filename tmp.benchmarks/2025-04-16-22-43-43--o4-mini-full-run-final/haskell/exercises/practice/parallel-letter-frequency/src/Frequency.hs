module Frequency (frequency) where

import Data.Char (isLetter)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel.Strategies (parList, rdeepseq, using)

-- | Count letter frequencies in a list of Texts using up to N parallel workers.
frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
  let ws = max 1 nWorkers
      size = (length texts + ws - 1) `div` ws
      chunks = chunkTexts ws size texts
      partials = map freqList chunks `using` parList rdeepseq
  in M.unionsWith (+) partials

-- Split the input list into at most 'workers' chunks of size up to 'size'
chunkTexts :: Int -> Int -> [a] -> [[a]]
chunkTexts _ _ [] = []
chunkTexts workers size xs = go xs workers
  where
    go ys 0 = [ys]
    go ys k =
      let (h, t) = splitAt size ys
      in h : go t (k - 1)

-- Compute the frequency map for a list of Texts
freqList :: [Text] -> Map Char Int
freqList = foldr (M.unionWith (+) . freqText) M.empty

-- Compute the frequency map for a single Text
freqText :: Text -> Map Char Int
freqText = T.foldl' update M.empty
  where
    update acc c
      | isLetter c = M.insertWith (+) c 1 acc
      | otherwise  = acc
