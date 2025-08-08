module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), rootLabel, subForest)
import Data.Maybe (listToMaybe)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree
    | rootLabel tree == x = Just tree
    | otherwise = do
        pathToX <- findPath x tree
        return $ reorient pathToX

-- Find path from root to target node
findPath :: Eq a => a -> Tree a -> Maybe [Tree a]
findPath target tree@(Node label children)
    | label == target = Just [tree]
    | otherwise = do
        childPath <- listToMaybe $ filter (/= Nothing) $ map (findPath target) children
        path <- childPath
        return (tree : path)

-- Reorient the tree along a path, making the last node in the path the new root
reorient :: Eq a => [Tree a] -> Tree a
reorient [] = error "Empty path"
reorient [node] = node
reorient (parent@(Node pLabel pChildren) : rest@(child : _)) =
    let reorientedRest = reorient rest
        Node newRootLabel newRootChildren = reorientedRest
        -- Remove the child from parent's children and add parent as child of the new root
        otherChildren = filter (\t -> rootLabel t /= rootLabel child) pChildren
        newParent = Node pLabel otherChildren
    in Node newRootLabel (newParent : newRootChildren)

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
    reorientedTree <- fromPOV from tree
    pathToTarget <- findPath to reorientedTree
    return $ map rootLabel pathToTarget
