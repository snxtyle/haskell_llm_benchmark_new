module Dominoes (chain) where

import Data.Maybe (listToMaybe)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = 
    let indices = [0 .. length dominoes - 1]
        orientations = [False, True]
        solutions = [ (firstDomino:chain') 
                    | i <- indices
                    , ori <- orientations
                    , let d = dominoes !! i
                          firstDomino = if ori then d else swap d
                          remaining = removeAt i dominoes
                    , Just chain' <- [build (fst firstDomino) (snd firstDomino) remaining]
                    ]
    in listToMaybe solutions
    where
        swap (a,b) = (b,a)

        removeAt :: Int -> [a] -> [a]
        removeAt i xs = take i xs ++ drop (i+1) xs

        build :: Int -> Int -> [(Int, Int)] -> Maybe [(Int, Int)]
        build start current remaining
            | null remaining = if current == start then Just [] else Nothing
            | otherwise = 
                let candidates = [ (dom, newRemaining) 
                                 | (idx, d) <- zip [0..] remaining
                                 , flipped <- [False, True]
                                 , let dom = if flipped then swap d else d
                                 , current == fst dom
                                 , let newRemaining = removeAt idx remaining
                                 ]
                    solutions = [ dom : rest 
                                | (dom, newRemaining) <- candidates
                                , Just rest <- [build start (snd dom) newRemaining]
                                ]
                in listToMaybe solutions
