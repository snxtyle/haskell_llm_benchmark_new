module Dominoes (chain) where

import Data.List (delete)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = findChain dominoes
  where
    findChain :: [(Int, Int)] -> Maybe [(Int, Int)]
    findChain stones = 
      let attempts = [tryBuildChain start (delete start stones) [start] 
                     | start <- stones]
      in firstJust attempts
    
    tryBuildChain :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
    tryBuildChain current remaining acc
      | null remaining = 
          let (firstNum, _) = head acc
              (_, lastNum) = current
          in if firstNum == lastNum
             then Just acc
             else Nothing
      | otherwise = 
          let (_, lastNum) = current
              candidates = [(stone, False) | stone <- remaining, fst stone == lastNum] ++
                          [(stone, True) | stone <- remaining, snd stone == lastNum]
              attempts = [tryNext stone flipped | (stone, flipped) <- candidates]
          in firstJust attempts
        where
          tryNext stone flipped =
            let oriented = if flipped then (snd stone, fst stone) else stone
                newRemaining = delete stone remaining
                newAcc = acc ++ [oriented]
            in tryBuildChain oriented newRemaining newAcc
    
    firstJust :: [Maybe a] -> Maybe a
    firstJust [] = Nothing
    firstJust (Just x : _) = Just x
    firstJust (Nothing : xs) = firstJust xs
