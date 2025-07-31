module Dominoes (chain) where

import Data.List (find)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = findChain dominoes

findChain :: [(Int, Int)] -> Maybe [(Int, Int)]
findChain [] = Just []
findChain dominoes = 
  case find isValidChain (permutationsAndOrientations dominoes) of
    Just chain -> Just chain
    Nothing -> Nothing

-- Generate all permutations and orientations of dominoes
permutationsAndOrientations :: [(Int, Int)] -> [[(Int, Int)]]
permutationsAndOrientations [] = [[]]
permutationsAndOrientations dominoes = 
  concatMap generateOrientations (permutations dominoes)
  where
    generateOrientations [] = [[]]
    generateOrientations (d:ds) = 
      let restOrientations = generateOrientations ds
          (a, b) = d
      in map ((a,b):) restOrientations ++ map ((b,a):) restOrientations

-- Generate all permutations of a list
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [y:zs | (y,ys) <- selections xs, zs <- permutations ys]

-- Generate all selections of one element from a list
selections :: [a] -> [(a,[a])]
selections [] = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]

-- Check if a chain is valid (adjacent matches and first/last match)
isValidChain :: [(Int, Int)] -> Bool
isValidChain [] = True
isValidChain [_] = True
isValidChain chain = 
  adjacentMatch chain && firstLastMatch chain

-- Check if adjacent dominoes have matching numbers
adjacentMatch :: [(Int, Int)] -> Bool
adjacentMatch [] = True
adjacentMatch [_] = True
adjacentMatch ((_,a):(b,c):rest) = 
  a == b && adjacentMatch ((b,c):rest)

-- Check if first and last dominoes have matching numbers
firstLastMatch :: [(Int, Int)] -> Bool
firstLastMatch [] = True
firstLastMatch [(_, _)] = True
firstLastMatch ((a,_):rest) = 
  let (_,lastNum) = last rest
  in a == lastNum
