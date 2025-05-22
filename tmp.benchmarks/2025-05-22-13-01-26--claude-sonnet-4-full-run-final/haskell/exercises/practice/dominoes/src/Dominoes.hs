module Dominoes (chain) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (find, delete)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = findChain dominoes

-- Build adjacency list representation of the graph
buildGraph :: [(Int, Int)] -> Map.Map Int [Int]
buildGraph dominoes = foldr addEdge Map.empty allEdges
  where
    allEdges = concatMap (\(a, b) -> [(a, b), (b, a)]) dominoes
    addEdge (from, to) graph = Map.insertWith (++) from [to] graph

-- Get all vertices in the graph
getVertices :: [(Int, Int)] -> [Int]
getVertices dominoes = Set.toList $ Set.fromList $ concatMap (\(a, b) -> [a, b]) dominoes

-- Check if all vertices have even degree (necessary for Eulerian cycle)
hasEulerianCycle :: [(Int, Int)] -> Bool
hasEulerianCycle dominoes = all evenDegree vertices
  where
    vertices = getVertices dominoes
    degreeMap = foldr (\(a, b) acc -> Map.insertWith (+) a 1 $ Map.insertWith (+) b 1 acc) Map.empty dominoes
    evenDegree v = even $ Map.findWithDefault 0 v degreeMap

-- Find a valid domino chain
findChain :: [(Int, Int)] -> Maybe [(Int, Int)]
findChain dominoes
  | not (hasEulerianCycle dominoes) = Nothing
  | otherwise = case getVertices dominoes of
      [] -> Just []
      (start:_) -> findPath start start dominoes []

-- Find a path that uses all dominoes and forms a cycle
findPath :: Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
findPath current start remaining path
  | null remaining && current == start = Just (reverse path)
  | null remaining = Nothing
  | otherwise = tryDominoes remaining
  where
    tryDominoes [] = Nothing
    tryDominoes (domino@(a, b):rest)
      | a == current = 
          case findPath b start (delete domino remaining) (domino : path) of
            Just result -> Just result
            Nothing -> tryDominoes rest
      | b == current = 
          case findPath a start (delete domino remaining) ((b, a) : path) of
            Just result -> Just result
            Nothing -> tryDominoes rest
      | otherwise = tryDominoes rest
