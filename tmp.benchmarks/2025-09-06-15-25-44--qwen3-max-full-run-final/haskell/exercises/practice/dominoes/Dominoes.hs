module Dominoes (chain) where

import Data.List (permutations, delete)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = findValidChain dominoes

findValidChain :: [(Int, Int)] -> Maybe [(Int, Int)]
findValidChain dominoes = case findChainStartingWith dominoes of
    Just result -> Just result
    Nothing -> case findChainStartingWith (map (\(a,b) -> (b,a)) dominoes) of
        Just result -> Just result
        Nothing -> Nothing

findChainStartingWith :: [(Int, Int)] -> Maybe [(Int, Int)]
findChainStartingWith dominoes = findChain (length dominoes) [head dominoes] (tail dominoes)

findChain :: Int -> [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
findChain targetLength currentChain remaining
    | length currentChain == targetLength =
        if isValidChain currentChain
        then Just currentChain
        else Nothing
    | otherwise = tryNextDominoes currentChain remaining

tryNextDominoes :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
tryNextDominoes currentChain [] = Nothing
tryNextDominoes currentChain remaining = 
    let lastVal = snd (last currentChain)
        candidates = [(d, False) | d <- remaining, fst d == lastVal] ++ 
                    [(d, True) | d <- remaining, snd d == lastVal]
    in tryCandidates currentChain remaining candidates

tryCandidates :: [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), Bool)] -> Maybe [(Int, Int)]
tryCandidates _ _ [] = Nothing
tryCandidates currentChain remaining (((a,b), flip):rest) =
    let nextDomino = if flip then (b,a) else (a,b)
        newChain = currentChain ++ [nextDomino]
        newRemaining = delete (a,b) remaining
        result = findChain (length currentChain + length remaining) newChain newRemaining
    in case result of
        Just chain -> Just chain
        Nothing -> tryCandidates currentChain remaining rest

isValidChain :: [(Int, Int)] -> Bool
isValidChain [] = True
isValidChain chain = fst (head chain) == snd (last chain)
