module Dominoes (chain) where

import Data.List (permutations)
import Data.Maybe (listToMaybe)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain xs = listToMaybe validChains
  where
    -- generate every permutation of the stones
    -- then every choice of orientations for that ordering
    -- and keep only those that form a valid circular chain
    validChains =
      [ candidate
      | perm <- permutations xs
      , candidate <- orientAll perm
      , isValidChain candidate
      ]

    orientAll :: [(Int, Int)] -> [[(Int, Int)]]
    orientAll = sequence . map orient

    orient :: (Int, Int) -> [(Int, Int)]
    orient (x,y)
      | x == y    = [(x,y)]
      | otherwise = [(x,y),(y,x)]

    -- check that adjacent halves match and
    -- that the chain is circular (last matches first)
    isValidChain :: [(Int, Int)] -> Bool
    isValidChain [] = True
    isValidChain ps@((l1,_):_) =
      let endOK = let (_, rn) = last ps in rn == l1
          adjOK = all (\((_, r),(l', _)) -> r == l') (zip ps (tail ps))
       in endOK && adjOK
