module Dominoes (chain) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, listToMaybe)

type Vertex = Int
type Edge = (Int, Int) -- store as given orientation
type Adj = M.Map Vertex (M.Map Vertex Int)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain ds =
  let adj = buildAdj ds
      nonZeroVerts = S.fromList [ v | (v, nbrs) <- M.toList adj, totalDeg nbrs > 0 ]
  in if S.null nonZeroVerts
        then Just [] -- no edges
        else
          let degOk = all (even . totalDeg) (M.elems adj)
              connOk = isConnected adj (S.findMin nonZeroVerts)
          in if not degOk || not connOk
                then Nothing
                else
                  let start = S.findMin nonZeroVerts
                      path = eulerianCircuit start adj
                      oriented = orientPath path ds
                  in if validates ds oriented then Just oriented else Nothing

-- Build adjacency with multiplicities
buildAdj :: [Edge] -> Adj
buildAdj = foldr addEdge M.empty
  where
    addEdge (u,v) = inc u v . inc v u
    inc a b = M.insertWith (const . M.insertWith (+) b 1) a (M.singleton b 1)
            . M.alter (Just . incInner) a
      where
        incInner Nothing  = M.singleton b 1
        incInner (Just m) = M.insertWith (+) b 1 m

totalDeg :: M.Map Vertex Int -> Int
totalDeg = sum

-- Connectivity ignoring multiplicities but considering only vertices with degree > 0
isConnected :: Adj -> Vertex -> Bool
isConnected adj start =
  let nonZero = S.fromList [ v | (v, nbrs) <- M.toList adj, totalDeg nbrs > 0 ]
      visited = dfs start S.empty
  in visited == nonZero
  where
    dfs v vis
      | S.member v vis = vis
      | otherwise =
          let vis' = S.insert v vis
              nbrs = [ u | (u,c) <- M.toList (M.findWithDefault M.empty v adj), c > 0 ]
          in foldr dfs vis' nbrs

-- Hierholzer's algorithm returning list of vertices in circuit order
eulerianCircuit :: Vertex -> Adj -> [Vertex]
eulerianCircuit start adj = reverse (go start adj [] [])
  where
    go v g stack path =
      case nextNeighbor v g of
        Nothing ->
          case stack of
            []       -> v:path
            (w, gw):stk -> go w gw stack (v:path)
        Just (u, g') -> go u g' ((v, g'):stack) path

    nextNeighbor :: Vertex -> Adj -> Maybe (Vertex, Adj)
    nextNeighbor v g =
      let nbrs = M.findWithDefault M.empty v g
      in case M.lookupMax nbrs of
           Nothing -> Nothing
           Just (u, cnt) ->
             let nbrs' = if cnt == 1 then M.delete u nbrs else M.insert u (cnt - 1) nbrs
                 g1 = M.insert v nbrs' g
                 nbrsU = M.findWithDefault M.empty u g1
                 cntVU = fromMaybe 0 (M.lookup v nbrsU)
                 nbrsU' = if cntVU <= 1 then M.delete v nbrsU else M.insert v (cntVU - 1) nbrsU
                 g2 = M.insert u nbrsU' g1
             in Just (u, g2)

-- Orient the sequence of vertices into dominoes and match against available pieces
orientPath :: [Vertex] -> [Edge] -> [(Int,Int)]
orientPath vs pieces = takeEdges edges orientedCounts
  where
    edges = zip vs (drop 1 vs)
    orientedCounts = buildPieceCounts pieces

    takeEdges [] _ = []
    takeEdges ((a,b):es) counts =
      case usePiece a b counts of
        Just (pab, counts') -> pab : takeEdges es counts'
        Nothing ->
          case usePiece b a counts of
            Just (pba, counts'') -> pba : takeEdges es counts''
            Nothing -> [] -- this should not happen if preconditions hold

-- Build counts of oriented pieces available, since pieces are directed in the output
buildPieceCounts :: [Edge] -> M.Map Edge Int
buildPieceCounts = foldr (\e -> M.insertWith (+) e 1) M.empty

usePiece :: Int -> Int -> M.Map Edge Int -> Maybe ((Int,Int), M.Map Edge Int)
usePiece a b counts =
  case M.lookup (a,b) counts of
    Just c | c > 0 ->
      let counts' = if c == 1 then M.delete (a,b) counts else M.insert (a,b) (c-1) counts
      in Just ((a,b), counts')
    _ -> Nothing

-- Validate that the oriented output uses exactly the multiset of input pieces
-- and forms a circular chain.
validates :: [Edge] -> [Edge] -> Bool
validates input out =
  let okLen = length input == length out
      okCircle = case out of
                   [] -> null input
                   xs -> let (a,_) = head xs
                             (_,z) = last xs
                         in a == z
      cnts xs = M.fromListWith (+) [ (e,1) | e <- xs ]
  in okLen && okCircle && cnts input == cnts out
