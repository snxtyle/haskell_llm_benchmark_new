module RailFenceCipher (encode, decode) where

import Data.List (sortOn)
import Data.Ord (Down(..))

encode :: Int -> String -> String
encode rails xs
  | rails <= 1 || rails >= length xs = xs
  | otherwise = concatMap snd $ railsBuckets rails xs

decode :: Int -> String -> String
decode rails xs
  | rails <= 1 || rails >= length xs = xs
  | otherwise = reconstruct rails xs

-- Helpers

-- Produce (railIndex, collectedCharsInOrder) buckets for encoding
railsBuckets :: Int -> String -> [(Int, String)]
railsBuckets n xs =
  let idxs = zigzagIndices n (length xs)        -- rail index for each position
      paired = zip idxs xs                      -- (railIndex, char)
      -- group by rail index preserving order
      buckets = foldr insertBucket [] paired
  in sortOn fst buckets
  where
    insertBucket :: (Int, Char) -> [(Int, String)] -> [(Int, String)]
    insertBucket (i, c) [] = [(i, [c])]
    insertBucket (i, c) ((j, s):rest)
      | i == j = (j, c : s) : rest
      | otherwise = (j, s) : insertBucket (i, c) rest

-- Reconstruct original text from encoded using zigzag pattern
reconstruct :: Int -> String -> String
reconstruct n xs =
  let len = length xs
      idxs = zigzagIndices n len                         -- rail index per position
      -- count how many positions per rail
      counts = railCounts n idxs
      -- split the encoded string into chunks per rail with those counts
      chunks = splitByCounts xs counts
      -- map from rail -> list of chars (queue) to pop from
      railMap = zip [0..n-1] chunks
      -- for each position, consume one char from corresponding rail
  in consume idxs railMap

-- Generate the zigzag rail index for each position 0..len-1
zigzagIndices :: Int -> Int -> [Int]
zigzagIndices n len
  | n <= 1 = replicate len 0
  | otherwise = take len $ cyclePath n
  where
    path = [0 .. n - 1] ++ [n - 2, n - 3 .. 1]
    cyclePath k = cycle path

-- Count how many positions belong to each rail
railCounts :: Int -> [Int] -> [Int]
railCounts n idxs =
  let initCounts = replicate n 0
  in foldr (\i acc -> increment i acc) initCounts idxs
  where
    increment i acc =
      let (a, x:b) = splitAt i acc
      in a ++ (x + 1) : b

-- Split a list into chunks with given sizes
splitByCounts :: [a] -> [Int] -> [[a]]
splitByCounts xs [] = []
splitByCounts xs (c:cs) =
  let (h, t) = splitAt c xs
  in h : splitByCounts t cs

-- Consume characters from rail chunks according to indices
consume :: [Int] -> [(Int, [Char])] -> [Char]
consume [] _ = []
consume (i:is) railMap =
  case pop i railMap of
    (c, railMap') -> c : consume is railMap'

-- Pop one character from the front of the list for rail i
pop :: Int -> [(Int, [Char])] -> (Char, [(Int, [Char])])
pop i [] = error "Invalid rail index"
pop i ((j, s):rest)
  | i == j =
      case s of
        []     -> error "Empty rail during reconstruction"
        (c:cs) -> (c, (j, cs) : rest)
  | otherwise =
      let (c, rest') = pop i rest
      in (c, (j, s) : rest')
