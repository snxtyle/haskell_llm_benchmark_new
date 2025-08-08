module Dominoes (chain) where

import Data.List (delete)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = findValidChain dominoes

-- Main function to find a valid domino chain
findValidChain :: [(Int, Int)] -> Maybe [(Int, Int)]
findValidChain dominoes = 
    -- Try starting with each domino in both orientations
    firstJust [tryBuildChain [d] (delete d dominoes) | d <- allStartingPositions]
  where
    allStartingPositions = dominoes ++ map swap dominoes
    swap (a, b) = (b, a)

-- Build a chain recursively using backtracking
tryBuildChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
tryBuildChain built [] = 
    -- All dominoes used, check if chain closes (first and last numbers match)
    if fst (head built) == snd (last built)
    then Just built
    else Nothing
tryBuildChain built remaining =
    -- Try to extend the chain with each remaining domino
    let lastNum = snd (last built)
        firstNum = fst (head built)
        candidates = findCandidates lastNum remaining
    in firstJust [extendChain built remaining d | d <- candidates]

-- Find dominoes that can connect to the given number
findCandidates :: Int -> [(Int, Int)] -> [(Int, Int)]
findCandidates num dominoes = 
    [(a, b) | (a, b) <- dominoes, a == num] ++
    [(b, a) | (a, b) <- dominoes, b == num]

-- Extend the chain with a domino
extendChain :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Maybe [(Int, Int)]
extendChain built remaining next =
    tryBuildChain (built ++ [next]) (delete (normalize next) remaining)
  where
    -- Normalize domino to its original form for deletion
    normalize (a, b) = if (a, b) `elem` remaining then (a, b) else (b, a)

-- Helper function to get the first Just value from a list
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : xs) = firstJust xs
