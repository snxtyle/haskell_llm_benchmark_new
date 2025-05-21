module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node, rootLabel, subForest), Forest)
import Data.List (filter, reverse, init, dropWhile)
import Data.Maybe (isJust)

-- Helper to find the path from the root to the target node.
-- Returns a list of Tree a nodes, from the root down to the target.
-- Each Tree a in the list retains its original subForest.
findPath :: Eq a => a -> Tree a -> Maybe [Tree a]
findPath target node@(Node val children)
  | val == target = Just [node]
  | otherwise =
      let
        foundInChild = [ path | child <- children, isJust (findPath target child), let Just path = findPath target child ]
      in
        case foundInChild of
          [] -> Nothing
          (path:_) -> Just (node : path)

-- Helper to build the parent branch for re-rooting.
-- `parentChain` is a list of Tree a nodes from the immediate parent of the current
-- re-rooted node up to the original root.
-- `childToExclude` is the Tree a node that was the child of `currentParentNode`
-- on the path to the target, and is now the root of the re-rooted subtree.
buildParentBranch :: Eq a => [Tree a] -> Tree a -> Tree a
buildParentBranch (currentParentNode:remainingPath) childToExclude =
    let
        -- Filter out the child that led to the re-rooted subtree
        childrenForCurrentParent = filter (\c -> rootLabel c /= rootLabel childToExclude) (subForest currentParentNode)
    in
        if null remainingPath
            -- If this is the original root, just attach its remaining children
            then Node (rootLabel currentParentNode) childrenForCurrentParent
            -- Otherwise, recursively build the next part of the parent branch
            else Node (rootLabel currentParentNode) (childrenForCurrentParent ++ [buildParentBranch remainingPath currentParentNode])
buildParentBranch [] _ = error "buildParentBranch: Empty parent chain should not happen here."


-- Reparents the tree on a selected node.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x originalTree =
    case findPath x originalTree of
        Nothing -> Nothing -- Target node not found
        Just path ->
            let
                targetTree = last path -- The target node itself
            in
                if null (init path) -- If path has only one element, x is the original root
                    then Just originalTree
                    else
                        let
                            -- The chain of parents from x's immediate parent up to the original root
                            parentChain = reverse (init path)
                            -- Build the branch representing the re-rooted parent chain
                            parentBranch = buildParentBranch parentChain targetTree
                        in
                            -- The new tree is rooted at x, with its original children
                            -- plus the newly constructed parent branch as a child.
                            Just (Node (rootLabel targetTree) (subForest targetTree ++ [parentBranch]))

-- Helper to find the path from the root to the target node, returning only the values.
findPathValues :: Eq a => a -> Tree a -> Maybe [a]
findPathValues target (Node val children)
  | val == target = Just [val]
  | otherwise =
      let
        foundInChild = [ path | child <- children, isJust (findPathValues target child), let Just path = findPathValues target child ]
      in
        case foundInChild of
          [] -> Nothing
          (path:_) -> Just (val : path)

-- Helper to find the longest common prefix of two paths, and the remaining unique parts.
findLCA :: Eq a => [a] -> [a] -> ([a], [a], [a])
findLCA [] p2 = ([], [], p2)
findLCA p1 [] = ([], p1, [])
findLCA (x:xs) (y:ys)
  | x == y = let (common, r1, r2) = findLCA xs ys in (x:common, r1, r2)
  | otherwise = ([], x:xs, y:ys)

-- Traces the path between two nodes in the tree.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree =
    let
        pathFromRootToFrom = findPathValues from tree
        pathFromRootToTo   = findPathValues to tree
    in
        case (pathFromRootToFrom, pathFromRootToTo) of
            (Just pFrom, Just pTo) ->
                let
                    -- Find the common prefix (path to LCA) and the remaining unique parts
                    (commonPrefix, restFrom, restTo) = findLCA pFrom pTo
                    -- The LCA is the last node in the common prefix
                    lca = last commonPrefix

                    -- Path from 'from' up to 'lca' (inclusive, then reversed)
                    -- e.g., if pFrom = [0,2,6], lca = 0, then dropWhile (/=0) gives [0,2,6], reverse gives [6,2,0]
                    pathFromToLCA = reverse (dropWhile (/= lca) pFrom)

                    -- Path from 'lca' down to 'to' (inclusive)
                    -- e.g., if pTo = [0,3,9], lca = 0, then dropWhile (/=0) gives [0,3,9]
                    pathToToLCA = dropWhile (/= lca) pTo
                in
                    -- Combine the two segments:
                    -- (from -> ... -> parent of LCA) ++ LCA ++ (child of LCA -> ... -> to)
                    -- `init pathFromToLCA` removes the LCA from the first segment to avoid duplication.
                    Just (init pathFromToLCA ++ pathToToLCA)
            _ -> Nothing -- One or both nodes not found
