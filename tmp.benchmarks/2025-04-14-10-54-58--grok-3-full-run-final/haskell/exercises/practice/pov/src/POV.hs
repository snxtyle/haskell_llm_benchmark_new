module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (listToMaybe, mapMaybe)

-- Reparent a tree from the perspective of a given node
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = 
    let path = findPath x tree
    in case path of
        Just p  -> Just $ buildTreeFromPath (reverse p) tree
        Nothing -> Nothing

-- Trace the path between two nodes in the tree
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
    -- Reparent tree from 'from' node perspective
    newTree <- fromPOV from tree
    -- Find path from root (which is now 'from') to 'to'
    let path = findPath to newTree
    return $ maybe [] id path

-- Find path from root to a node in the tree
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath x (Node value forest)
    | value == x = Just [value]
    | otherwise  = do
        subPath <- listToMaybe $ mapMaybe (findPath x) forest
        return $ value : subPath

-- Build a new tree by reversing the parent-child relationships along the path
buildTreeFromPath :: Eq a => [a] -> Tree a -> Tree a
buildTreeFromPath [] tree = tree
buildTreeFromPath [x] tree = tree
buildTreeFromPath (x:xs) originalTree =
    let (Node rootValue forest) = originalTree
        childTree = findSubTree x originalTree
        remainingForest = filter (not . isRoot x) forest
    in case childTree of
        Just t@(Node childValue childForest) ->
            let parentTree = Node rootValue (filter (not . isRoot childValue) forest)
                newSubTree = buildTreeFromPath xs t
            in Node x (parentTree : childForest)
        Nothing -> error "Invalid path in buildTreeFromPath"

-- Find a subtree with given root value
findSubTree :: Eq a => a -> Tree a -> Maybe (Tree a)
findSubTree x (Node value forest)
    | value == x = Just (Node value forest)
    | otherwise  = listToMaybe $ mapMaybe (findSubTree x) forest

-- Check if a tree's root matches a value
isRoot :: Eq a => a -> Tree a -> Bool
isRoot x (Node value _) = x == value
