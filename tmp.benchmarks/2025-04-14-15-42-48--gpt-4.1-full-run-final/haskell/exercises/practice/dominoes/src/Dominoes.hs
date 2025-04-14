module Dominoes (chain) where

import Data.List (delete)

-- Try to build a chain starting with the given chain and remaining dominoes
buildChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
buildChain chain [] =
    -- If no dominoes left, check if the chain is closed
    if null chain then Just [] -- empty input is a valid chain
    else if fst (head chain) == snd (last chain) then Just chain
    else Nothing
buildChain [] dominoes =
    -- Try each domino as the starting domino
    tryDominoes dominoes
  where
    tryDominoes [] = Nothing
    tryDominoes (d:ds) =
        case buildChain [d] (delete d dominoes) of
            Just c -> Just c
            Nothing ->
                let d' = swap d
                in if d /= d'
                    then case buildChain [d'] (delete d dominoes) of
                        Just c -> Just c
                        Nothing -> tryDominoes ds
                    else tryDominoes ds
buildChain chain dominoes =
    let end = snd (last chain)
        tryNext [] = Nothing
        tryNext (d:ds) =
            if fst d == end
                then case buildChain (chain ++ [d]) (delete d dominoes) of
                    Just c -> Just c
                    Nothing -> tryNext ds
            else if snd d == end
                then case buildChain (chain ++ [swap d]) (delete d dominoes) of
                    Just c -> Just c
                    Nothing -> tryNext ds
            else tryNext ds
    in tryNext dominoes

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain dominoes = buildChain [] dominoes
