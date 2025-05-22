module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (listToMaybe)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = 
    case findPath x tree of
        Nothing -> Nothing
        Just path -> Just $ reparent path tree

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
    reparented <- fromPOV from tree
    pathToTarget <- findPath to reparented
    return $ map rootLabel pathToTarget

-- Find the path from root to target node
findPath :: Eq a => a -> Tree a -> Maybe [Tree a]
findPath target node@(Node value children)
    | value == target = Just [node]
    | otherwise = do
        childPaths <- listToMaybe $ filter (not . null) $ map (findPath target) children
        case childPaths of
            Nothing -> Nothing
            Just path -> Just (node : path)

-- Reparent the tree based on the path to the new root
reparent :: Eq a => [Tree a] -> Tree a -> Tree a
reparent [] tree = tree
reparent [target] _ = target
reparent (parent:child:rest) originalTree =
    let newChild = reparent (child:rest) originalTree
        Node childValue childChildren = newChild
        -- Remove the child from parent's children and add parent as child's child
        Node parentValue parentChildren = parent
        filteredChildren = filter (\(Node v _) -> v /= childValue) parentChildren
        newParent = Node parentValue filteredChildren
    in Node childValue (newParent : childChildren)
