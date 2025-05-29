module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable (foldl')

buildGraph :: Ord a => Tree a -> M.Map a [a]
buildGraph tree = go tree M.empty
  where
    go (Node x children) acc =
        let childLabels = map rootLabel children
            acc1 = M.insertWith (++) x childLabels acc
            acc2 = foldl' (\g child -> M.insertWith (++) (rootLabel child) [x] g) acc1 children
            acc3 = foldl' (\g child -> go child g) acc2 children
        in acc3

fromPOV :: Ord a => a -> Tree a -> Maybe (Tree a)
fromPOV node tree = 
    let graph = buildGraph tree
    in if M.member node graph
       then Just $ buildTree node graph
       else Nothing
  where
    buildTree start graph = 
        let 
            go parent x = 
                let neighbors = M.findWithDefault [] x graph
                    childrenNodes = case parent of
                                     Nothing -> neighbors
                                     Just p  -> filter (/= p) neighbors
                in Node x $ map (go (Just x)) childrenNodes
        in go Nothing start

tracePathBetween :: Ord a => a -> a -> Tree a -> Maybe [a]
tracePathBetween start end tree = 
    let graph = buildGraph tree
    in if not (M.member start graph) || not (M.member end graph)
       then Nothing
       else bfs start end graph
  where
    bfs a b graph = 
        let 
            loop visited [] = Nothing
            loop visited ((node, path):rest) 
                | node == b = Just path
                | otherwise = 
                    let neighbors = M.findWithDefault [] node graph
                        validNeighbors = filter (`S.notMember` visited) neighbors
                        newVisited = S.union visited (S.fromList validNeighbors)
                        newQueue = rest ++ [ (n, path ++ [n]) | n <- validNeighbors ]
                    in loop newVisited newQueue
        in loop (S.singleton a) [(a, [a])]
