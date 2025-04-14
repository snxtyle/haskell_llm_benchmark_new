module Dominoes (chain) where

import Data.List (nub)

type Graph = [(Int, [Int])]  -- List of (node, list of neighbors)

-- Add an edge to the graph
addEdge :: Graph -> (Int, Int) -> Graph
addEdge graph (a, b) = updateGraph (updateGraph graph a b) b a

-- Update a node's neighbors in the graph
updateGraph :: Graph -> Int -> Int -> Graph
updateGraph [] node neighbor = [(node, [neighbor])]
updateGraph ((n, ns):rest) node neighbor
  | n == node  = (n, neighbor : ns) : rest
  | otherwise  = (n, ns) : updateGraph rest node neighbor

-- Get the neighbors of a node
getNeighbors :: Graph -> Int -> [Int]
getNeighbors graph node = case lookup node graph of
    Just ns -> ns
    Nothing -> []

-- Get all unique nodes from the dominoes
getNodes :: [(Int, Int)] -> [Int]
getNodes dominoes = nub $ concatMap (\(a, b) -> [a, b]) dominoes

-- Compute the degree of each node
degrees :: Graph -> [Int] -> [Int]
degrees graph nodes = map (\node -> length (getNeighbors graph node)) nodes

-- Check if the graph is connected using BFS
isConnected :: Graph -> [Int] -> Bool
isConnected graph nodes =
    if null nodes then True  -- Empty graph is connected
    else
        let bfs queue visited = case queue of
                [] -> visited
                (current:qs) ->
                    let neighbors = getNeighbors graph current
                        newNeighbors = filter (`notElem` visited) neighbors
                    in bfs (qs ++ newNeighbors) (current : visited)
            reachable = bfs [head nodes] []  -- Start from the first node
        in length (filter (`elem` reachable) nodes) == length nodes  -- All nodes reachable?

-- Hierholzer's algorithm to find Eulerian cycle
eulerianCycle :: Graph -> Int -> Maybe [(Int, Int)]
eulerianCycle graph start =
    let go current g =
            case getNeighbors g current of
                [] -> ([], g)  -- No neighbors, return empty path
                neighbors ->
                    let (path, g') = foldl
                            (\(pathsAcc, gAcc) neighbor ->
                                let (subPath, gSub) = go neighbor (updateGraph gAcc current neighbor)  -- Remove edge and recurse
                                in (pathsAcc ++ subPath, gSub)
                            ) ([], g) neighbors
                    in (reverse path ++ [(current, head neighbors)], g')  -- Add current edge and reverse subpath
        fullPath = fst $ go start graph
    in if null fullPath then Nothing else Just fullPath  -- If no path, return Nothing

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []  -- Empty list is a valid chain
chain dominoes =
    let graph = foldl addEdge [] dominoes  -- Build the graph
        nodes = getNodes dominoes
        degs = degrees graph nodes
    in if all even degs && isConnected graph nodes  -- All degrees even and connected?
       then case nodes of  -- Find Eulerian cycle from the first node
               [] -> Just []  -- No nodes
               (start:_) -> eulerianCycle graph start
       else Nothing  -- Not possible
