module RailFenceCipher (encode, decode) where

import Data.List (sortOn)

encode :: Int -> String -> String
encode n s
  | n <= 1 = s
  | otherwise = concatMap (map snd . filter ((==r) . fst)) indexed
  where
    cycleLen = 2 * n - 2
    indexed = zip (map railIndex [0..]) s
    railIndex i =
      let pos = i `mod` cycleLen
      in if pos < n then pos else cycleLen - pos

decode :: Int -> String -> String
decode n s
  | n <= 1 = s
  | otherwise = map snd $ sortByIndex $ concat railsWithIndices
  where
    len = length s
    cycleLen = 2 * n - 2

    -- For each position, compute its rail index
    railIndices = map railIndex [0..len-1]
    railIndex i =
      let pos = i `mod` cycleLen
      in if pos < n then pos else cycleLen - pos

    -- Count how many chars per rail
    counts = [length $ filter (==r) railIndices | r <- [0..n-1]]

    -- Split the input string into rails according to counts
    rails = splitByCounts counts s

    -- For each rail, pair each char with its original index in the message
    railsWithIndices = zipWith (\r cs -> zip (positionsOf r) cs) [0..] rails

    -- Positions of each rail in the original message
    positionsOf r = [i | (i, rail) <- zip [0..] railIndices, rail == r]

    -- Sort by original index to reconstruct original message order
    sortByIndex = sortOn fst

-- Helper: split a list into parts with given lengths
splitByCounts :: [Int] -> [a] -> [[a]]
splitByCounts [] _ = []
splitByCounts (c:cs) xs = let (h,t) = splitAt c xs in h : splitByCounts cs t
