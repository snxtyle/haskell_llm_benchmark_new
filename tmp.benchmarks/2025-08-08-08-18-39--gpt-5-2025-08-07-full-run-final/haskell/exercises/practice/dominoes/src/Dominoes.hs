module Dominoes (chain) where

import Data.List (foldl', sort)

-- Adjacency multigraph using association lists:
-- For each vertex, a list of (neighbor, multiplicity)
type Adj = [(Int, [(Int, Int)])]

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain ds =
  let adj = buildAdj ds
  in if not (connected adj) || not (evenDegrees adj)
        then Nothing
        else
          let start = minimum (map fst adj)
              vs = eulerianCycle adj start
              es = zip vs (drop 1 vs)
          in if length es == length ds
                then Just es
                else Nothing

-- Build an undirected multigraph adjacency structure from the domino list
buildAdj :: [(Int, Int)] -> Adj
buildAdj = foldl' (\a (x,y) -> addEdge x y a) []

addEdge :: Int -> Int -> Adj -> Adj
addEdge x y
  | x == y    = modifyNeighbors x (updateCount y (+1))
  | otherwise = modifyNeighbors y (updateCount x (+1)) . modifyNeighbors x (updateCount y (+1))

-- Remove one instance of the edge between v and u
decEdge :: Int -> Int -> Adj -> Adj
decEdge v u
  | v == u    = modifyNeighbors v (updateCount u (subtract 1))
  | otherwise = modifyNeighbors u (updateCount v (subtract 1)) . modifyNeighbors v (updateCount u (subtract 1))

-- Modify the neighbor map for a vertex
modifyNeighbors :: Int -> ([(Int, Int)] -> [(Int, Int)]) -> Adj -> Adj
modifyNeighbors v f adj =
  let nm  = lookupNeighbors v adj
      nm' = f nm
  in replaceOrInsert v nm' adj

lookupNeighbors :: Int -> Adj -> [(Int, Int)]
lookupNeighbors v adj =
  case lookup v adj of
    Just nm -> nm
    Nothing -> []

replaceOrInsert :: Int -> [(Int, Int)] -> Adj -> Adj
replaceOrInsert v nm [] = if null nm then [] else [(v, nm)]
replaceOrInsert v nm ((k,m):xs)
  | k == v    = if null nm then xs else (v, nm) : xs
  | otherwise = (k, m) : replaceOrInsert v nm xs

updateCount :: Int -> (Int -> Int) -> [(Int, Int)] -> [(Int, Int)]
updateCount u f nm =
  let c   = case lookup u nm of
              Just z  -> z
              Nothing -> 0
      c'  = f c
      nm' = filter ((/= u) . fst) nm
  in if c' <= 0 then nm' else (u, c') : nm'

-- Check if graph is connected when restricted to vertices with non-zero degree
connected :: Adj -> Bool
connected adj
  | null adj  = True
  | otherwise =
      let start   = minimum (map fst adj)
          visited = dfs start []
      in uniqueSorted visited == uniqueSorted (map fst adj)
  where
    dfs v seen
      | v `elem` seen = seen
      | otherwise =
          let nbrs = map fst (lookupNeighbors v adj)
          in foldl' (flip dfs) (v : seen) nbrs

-- All vertices must have even degree for an Eulerian circuit
evenDegrees :: Adj -> Bool
evenDegrees adj =
  all (\(v, nm) -> even (degOf v nm)) adj

-- Degree of a vertex: sum of multiplicities, counting self-loops twice
degOf :: Int -> [(Int, Int)] -> Int
degOf v nm =
  let s    = sum (map snd nm)
      self = case lookup v nm of
               Just c  -> c
               Nothing -> 0
  in s + self

-- Hierholzer's algorithm to construct an Eulerian cycle (as a list of vertices)
eulerianCycle :: Adj -> Int -> [Int]
eulerianCycle adj0 start = path
  where
    (path, _) = go [start] [] adj0

    go :: [Int] -> [Int] -> Adj -> ([Int], Adj)
    go [] acc a = (acc, a)
    go (v:stack) acc a =
      case pickNext (lookupNeighbors v a) of
        Nothing -> go stack (v:acc) a
        Just u  -> go (u:v:stack) acc (decEdge v u a)

    -- pick any available neighbor (deterministic: smallest key)
    pickNext :: [(Int, Int)] -> Maybe Int
    pickNext [] = Nothing
    pickNext nm = Just (minimum (map fst nm))

-- Helpers
uniqueSorted :: [Int] -> [Int]
uniqueSorted = uniq . sort
  where
    uniq [] = []
    uniq (x:xs) = x : uniq (dropWhile (== x) xs)
