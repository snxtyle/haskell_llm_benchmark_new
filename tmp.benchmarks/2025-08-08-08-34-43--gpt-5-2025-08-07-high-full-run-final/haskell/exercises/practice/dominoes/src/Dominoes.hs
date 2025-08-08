module Dominoes (chain) where

import Data.List (find, nub)

type Domino = (Int, Int)
type EdgeId = Int
type Adj = [(Int, [(Int, EdgeId)])]

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain dominoes
  | null dominoes = Just []
  | not (allEvenDegrees dominoes) = Nothing
  | not (isConnected dominoes) = Nothing
  | otherwise =
      let adj = buildAdj dominoes
          start = fst (head dominoes)
          verts = euler start adj
          result = zip verts (tail verts)
      in if length result == length dominoes
           then Just result
           else Nothing

-- Check that every vertex appearing in the dominoes has even degree.
allEvenDegrees :: [Domino] -> Bool
allEvenDegrees ds = all (even . snd) (degreeTable ds)

degreeTable :: [Domino] -> [(Int, Int)]
degreeTable = foldl addDeg []
  where
    addDeg :: [(Int, Int)] -> Domino -> [(Int, Int)]
    addDeg tbl (a, b) = incCount b 1 (incCount a 1 tbl)

incCount :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
incCount key delta [] = [(key, delta)]
incCount key delta ((k, v) : rest)
  | k == key  = (k, v + delta) : rest
  | otherwise = (k, v) : incCount key delta rest

-- Check that the graph formed by the dominoes is connected if we ignore isolated vertices.
isConnected :: [Domino] -> Bool
isConnected [] = True
isConnected ds =
  let verts = vertices ds
      start = fst (head ds)
      visited = dfs ds [start] []
  in all (`elem` visited) verts

vertices :: [Domino] -> [Int]
vertices ds = nub (concatMap (\(a, b) -> [a, b]) ds)

dfs :: [Domino] -> [Int] -> [Int] -> [Int]
dfs _ [] visited = visited
dfs ds (x:xs) visited
  | x `elem` visited = dfs ds xs visited
  | otherwise =
      let ns = neighborsOf ds x
      in dfs ds (ns ++ xs) (x : visited)

neighborsOf :: [Domino] -> Int -> [Int]
neighborsOf ds v = nub $ concatMap pick ds
  where
    pick (a, b)
      | a == v && b == v = [v]       -- loop
      | a == v           = [b]
      | b == v           = [a]
      | otherwise        = []

-- Build adjacency list with edge IDs to handle duplicates.
buildAdj :: [Domino] -> Adj
buildAdj ds = foldl addEdge [] (zip [0..] ds)
  where
    addEdge :: Adj -> (EdgeId, Domino) -> Adj
    addEdge adj (eid, (a, b)) =
      insertAdj a (b, eid) (insertAdj b (a, eid) adj)

insertAdj :: Int -> (Int, EdgeId) -> Adj -> Adj
insertAdj key val [] = [(key, [val])]
insertAdj key val ((k, vs) : rest)
  | key == k  = (k, val : vs) : rest
  | otherwise = (k, vs) : insertAdj key val rest

-- Hierholzer's algorithm for undirected multigraph to produce an Eulerian circuit.
-- Returns the sequence of vertices; length is edges + 1 and first == last.
euler :: Int -> Adj -> [Int]
euler start adj = go [] start [] []
  where
    go :: [Int] -> Int -> [EdgeId] -> [Int] -> [Int]
    go stack v used circuit =
      case nextUnusedEdge v used of
        Just (u, eid) -> go (v : stack) u (eid : used) circuit
        Nothing ->
          case stack of
            (prev : rest) -> go rest prev used (v : circuit)
            []            -> v : circuit

    nextUnusedEdge :: Int -> [EdgeId] -> Maybe (Int, EdgeId)
    nextUnusedEdge v used =
      let ns = neighbors v
      in find (\(_, eid) -> not (eid `elem` used)) ns

    neighbors :: Int -> [(Int, EdgeId)]
    neighbors v = case lookup v adj of
      Just ns -> ns
      Nothing -> []
