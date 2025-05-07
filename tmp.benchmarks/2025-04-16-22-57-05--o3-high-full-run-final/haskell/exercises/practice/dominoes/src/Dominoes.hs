module Dominoes (chain) where

import           Data.List (foldl', nub, sort)

type Domino    = (Int, Int)
type EdgeId    = Int
type Adjacency = [(Int, [(EdgeId, Int)])]   -- vertex -> [(edgeId, neighbour)]

-- | Attempt to build a closed domino chain which uses every given domino
--   exactly once (each domino can be flipped).
--
--   Returns `Nothing` if no such chain exists.
chain :: [Domino] -> Maybe [Domino]
chain [] = Just []
chain ds
  | not (allEvenDegrees degMap)     = Nothing
  | not (isConnected firstV adj)    = Nothing
  | otherwise =
      let vertices = hierholzer adj firstV
          edges    = toEdges vertices
      in if length edges == length ds
            then Just edges
            else Nothing
  where
    firstV  = fst (head ds)
    adj     = buildAdjacency ds
    degMap  = buildDegreeMap ds

-- ---------------------------------------------------------------------
--  Degree map helpers
-- ---------------------------------------------------------------------

buildDegreeMap :: [Domino] -> [(Int, Int)]
buildDegreeMap = foldl' add []
  where
    add m (a, b)
      | a == b    = inc a 2 m
      | otherwise = inc b 1 (inc a 1 m)

    inc :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
    inc k v [] = [(k, v)]
    inc k v ((x, n) : xs)
      | k == x    = (x, n + v) : xs
      | otherwise = (x, n) : inc k v xs

allEvenDegrees :: [(Int, Int)] -> Bool
allEvenDegrees = all (even . snd)

-- ---------------------------------------------------------------------
--  Connectivity (depth‑first search)
-- ---------------------------------------------------------------------

isConnected :: Int -> Adjacency -> Bool
isConnected _ [] = True
isConnected start adj = sort (nub visited) == sort vertices
  where
    vertices = map fst adj

    visited  = dfs start []
    dfs v seen
      | v `elem` seen = seen
      | otherwise     =
          let neighs = map snd (findNeighbours adj v)
          in foldl' (flip dfs) (v : seen) neighs

-- ---------------------------------------------------------------------
--  Building and mutating the adjacency list (multi‑graph)
-- ---------------------------------------------------------------------

buildAdjacency :: [Domino] -> Adjacency
buildAdjacency = foldl' addEdge [] . zip [0 ..]
  where
    addEdge :: Adjacency -> (EdgeId, Domino) -> Adjacency
    addEdge adj (eid, (a, b))
      | a == b    = addNeighbour a (eid, b) adj
      | otherwise = addNeighbour b (eid, a) (addNeighbour a (eid, b) adj)

    addNeighbour :: Int -> (EdgeId, Int) -> Adjacency -> Adjacency
    addNeighbour v pair [] = [(v, [pair])]
    addNeighbour v pair ((x, ps) : xs)
      | v == x    = (x, pair : ps) : xs
      | otherwise = (x, ps) : addNeighbour v pair xs

findNeighbours :: Adjacency -> Int -> [(EdgeId, Int)]
findNeighbours [] _ = []
findNeighbours ((v, ns) : xs) target
  | v == target = ns
  | otherwise   = findNeighbours xs target

updateVertex
  :: Int
  -> ([(EdgeId, Int)] -> [(EdgeId, Int)])
  -> Adjacency
  -> Adjacency
updateVertex v f [] = [(v, f [])]
updateVertex v f ((x, ns) : xs)
  | v == x    = (x, f ns) : xs
  | otherwise = (x, ns) : updateVertex v f xs

removeEdge :: Int -> EdgeId -> Int -> Adjacency -> Adjacency
removeEdge v eid u adj =
  let strip = filter ((/= eid) . fst)
      adj'  = updateVertex v strip adj
  in if v == u
        then adj'
        else updateVertex u strip adj'

-- ---------------------------------------------------------------------
--  Hierholzer’s algorithm to obtain Eulerian circuit
-- ---------------------------------------------------------------------

hierholzer :: Adjacency -> Int -> [Int]
hierholzer initialAdj start = go [start] [] initialAdj
  where
    go [] circuit _   = circuit
    go (v : stk) circuit adj =
      case findNeighbours adj v of
        [] -> go stk (v : circuit) adj
        ((eid, u) : _) ->
          let adj' = removeEdge v eid u adj
              stk' = u : v : stk
          in go stk' circuit adj'

-- ---------------------------------------------------------------------
--  Convert vertex circuit into oriented domino edges
-- ---------------------------------------------------------------------

toEdges :: [Int] -> [Domino]
toEdges vs = case reverse vs of
               []        -> []
               [_]       -> []
               lst       -> zip lst (tail lst)
