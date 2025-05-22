module Dominoes (chain) where

import Data.List (delete)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = findChain dominoes

findChain :: [(Int, Int)] -> Maybe [(Int, Int)]
findChain [] = Just []
findChain (first:rest) = 
    case tryChain first rest of
        Just result -> Just result
        Nothing -> tryChain (swap first) rest
  where
    tryChain start remaining = 
        case buildChain (snd start) (fst start) remaining [start] of
            [] -> Nothing
            (result:_) -> Just result

buildChain :: Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
buildChain target start [] acc
    | target == start = [reverse acc]
    | otherwise = []
buildChain target start remaining acc = 
    concatMap tryDomino remaining
  where
    tryDomino domino = 
        let rest = delete domino remaining
        in if fst domino == target
           then buildChain (snd domino) start rest (domino : acc)
           else if snd domino == target
                then buildChain (fst domino) start rest (swap domino : acc)
                else []

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
