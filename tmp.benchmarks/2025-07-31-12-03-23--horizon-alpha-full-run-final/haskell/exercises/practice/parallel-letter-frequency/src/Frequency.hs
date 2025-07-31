module Frequency (frequency) where

import Control.Parallel.Strategies (parList, rdeepseq, withStrategy)
import Data.Char (isAlpha, toLower)
import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | null texts = M.empty
  | nWorkers <= 1 = foldl' (M.unionWith (+)) M.empty (map countText texts)
  | otherwise =
      let chunks = chunk nWorkers texts
          partials = withStrategy (parList rdeepseq) (map (foldl' step M.empty) chunks)
      in foldl' (M.unionWith (+)) M.empty partials
  where
    step :: Map Char Int -> Text -> Map Char Int
    step acc t = M.unionWith (+) acc (countText t)

countText :: Text -> Map Char Int
countText = T.foldl' addChar M.empty . T.toLower
  where
    addChar :: Map Char Int -> Char -> Map Char Int
    addChar m c
      | isAlpha c = M.insertWith (+) c 1 m
      | otherwise = m

chunk :: Int -> [a] -> [[a]]
chunk k xs
  | k <= 1 = [xs]
  | otherwise = go sizes xs
  where
    n = length xs
    base = n `div` k
    extra = n `mod` k
    sizes = replicate extra (base + 1) ++ replicate (k - extra) base
    go [] ys = [ys | not (null ys)]
    go (s:ss) ys =
      let (a,b) = splitAt s ys
      in if s == 0 then go ss ys else a : go ss b
