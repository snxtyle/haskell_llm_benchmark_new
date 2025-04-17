module Dominoes (chain) where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Data.List       (foldl')

--------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------
-- | Attempt to reorder the given list of dominoes so that they form a
--   closed chain (the first number equals the last one) and neighbouring
--   dominoes touch on identical numbers.
--
--   The function returns:
--     * @Just xs@ – a valid chain that uses exactly the supplied dominoes.
--     * @Nothing@ – when no such chain exists.
--
--   Dominoes may be flipped, duplicates are allowed.
chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain [d@(a,b)]
  | a == b    = Just [d]          -- single doubled domino forms a loop
  | otherwise = Nothing
chain dominoes
  | not (allEvenDegrees g)     = Nothing
  | not (isConnected g)        = Nothing
  | otherwise =
      let start   = fst (head dominoes)
          verts   = eulerianCycle g start
      in case verts of
           [] -> Nothing
           vs ->
             let ds = zip vs (tail vs)
             in if length ds == length dominoes
                   then Just ds
                   else Nothing
  where
    g = buildGraph dominoes

--------------------------------------------------------------------
-- Graph representation helpers
--------------------------------------------------------------------
-- A multigraph where multiple edges between two vertices are allowed.
-- We store it as  Map v (Map v countOfEdges)
type Graph = M.Map Int (M.Map Int Int)

buildGraph :: [(Int,Int)] -> Graph
buildGraph = foldl' (\acc (a,b) -> addEdge a b acc) M.empty

addEdge :: Int -> Int -> Graph -> Graph
addEdge a b = addHalf a b . addHalf b a
  where
    addHalf from to =
      M.insertWith
        (M.unionWith (+))
        from
        (M.singleton to 1)

removeEdge :: Int -> Int -> Graph -> Graph
removeEdge a b = remHalf a b . remHalf b a
  where
    remHalf from to =
      M.update adjust from
      where
        adjust m =
          case M.updateLookupWithKey (\_ c -> if c > 1 then Just (c-1) else Nothing) to m of
            (Nothing, m') | M.null m' -> Nothing
            (_,       m')             -> Just m'

vertexDegree :: M.Map Int Int -> Int
vertexDegree = sum . M.elems

allEvenDegrees :: Graph -> Bool
allEvenDegrees = all (even . vertexDegree . snd) . M.toList

--------------------------------------------------------------------
-- Connectivity
--------------------------------------------------------------------
isConnected :: Graph -> Bool
isConnected g
  | null nonZero = True                    -- No edges -> vacuously connected
  | otherwise    = S.fromList nonZero == dfs (head nonZero) S.empty
  where
    nonZero = [v | (v,neigh) <- M.toList g, not (M.null neigh)]
    dfs v visited
      | v `S.member` visited = visited
      | otherwise            = foldl' (flip dfs) (S.insert v visited) neighbours
      where
        neighbours = M.keys (M.findWithDefault M.empty v g)

--------------------------------------------------------------------
-- Eulerian cycle (Hierholzer)
--------------------------------------------------------------------
eulerianCycle :: Graph -> Int -> [Int]
eulerianCycle g0 start = go [start] g0 []
  where
    go [] _ path = reverse path
    go (v:stack) g path =
      case nextNeighbour v g of
        Nothing    -> go stack g (v:path)        -- dead end, add to output
        Just (w,g') -> go (w:v:stack) g' path    -- traverse edge, stay on v
    nextNeighbour v g = do
      neighMap <- M.lookup v g
      (w,_) <- M.lookupMin neighMap
      let g' = removeEdge v w g
      pure (w,g')
