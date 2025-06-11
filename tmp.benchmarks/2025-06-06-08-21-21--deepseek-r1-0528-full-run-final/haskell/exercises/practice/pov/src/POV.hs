module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import qualified Data.Map as Map
import qualified Data.Set as Set

edgesOfTree :: Tree a -> [(a, a)]
edgesOfTree (Node r children) = concatMap forChild children ++ concatMap edgesOfTree children
  where forChild child = [(r, rootLabel child), (rootLabel child, r)]

buildGraph :: Ord a => Tree a -> Map.Map a [a]
buildGraph tree = foldr insertEdge Map.empty (edgesOfTree tree)
  where
    insertEdge (u,v) = Map.insertWith (++) u [v]

fromPOV :: Ord a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree =
  let graph = buildGraph tree
      nodes = Map.keysSet graph
   in if Set.member x nodes
        then Just (buildTree graph x)
        else Nothing
  where
    buildTree graph start = go Set.empty start
      where
        go seen x = 
          let seen' = Set.insert x seen
              childrenLabels = filter (`Set.notMember` seen') (graph Map.! x)
          in Node x (map (go seen') childrenLabels)


tracePathBetween :: Ord a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree =
  let graph = buildGraph tree
      vertices = Map.keysSet graph
   in if not (Set.member from vertices) || not (Set.member to vertices)
        then Nothing
        else 
          let init = [[from]]
              loop [] = Nothing
              loop (p:qs)
                | head p == to = Just (reverse p)
                | otherwise =
                    let current = head p
                        neighbors = graph Map.! current
                        unvisited = filter (`notElem` p) neighbors
                        newPaths = map (:p) unvisited
                    in loop (qs ++ newPaths)
          in loop init
