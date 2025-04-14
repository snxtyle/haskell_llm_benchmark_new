module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node, rootLabel, subForest))
import Data.Maybe (listToMaybe, mapMaybe)

-- Represents the path from the root to a focused node (like a zipper)
-- Stores (Parent Label, Left Siblings, Right Siblings) for each level
type PathContext a = [(a, [Tree a], [Tree a])]

-- Represents the location of a focused node within the tree
type Location a = (Tree a, PathContext a)

-- Find the location of a node with the given value
findLoc :: Eq a => a -> Tree a -> Maybe (Location a)
findLoc target tree = go [] tree
  where
    go :: Eq a => PathContext a -> Tree a -> Maybe (Location a)
    go context current@(Node label children)
      | label == target = Just (current, context) -- Found the target
      | otherwise       = listToMaybe $ mapMaybe (findChild context current) (zip [0..] children)

    findChild :: Eq a => PathContext a -> Tree a -> (Int, Tree a) -> Maybe (Location a)
    findChild context parent (idx, child) =
        let (Node pLabel children) = parent
            (leftSiblings, _:rightSiblings) = splitAt idx children
            newContext = (pLabel, leftSiblings, rightSiblings) : context
        in go newContext child

-- Reconstruct the tree upwards from a given location, making the focused node the root
goUp :: Tree a -> PathContext a -> Tree a
goUp currentTree [] = currentTree -- Already at the original root
goUp currentTree ((pLabel, ls, rs) : ctx) =
    let newParentNode = Node pLabel (ls ++ rs) -- Parent node without the current subtree
        newCurrentTree = Node (rootLabel currentTree) (newParentNode : subForest currentTree)
    in goUp newCurrentTree ctx

-- Re-parent the tree onto the given node 'x'.
-- Returns Nothing if 'x' is not found in the tree.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = case findLoc x tree of
    Nothing -> Nothing -- Target node 'x' not found
    Just (focus, context) -> Just $ goUp focus context

-- Find the path from the root of a tree to the target node.
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath target (Node label children)
    | label == target = Just [label] -- Found the target at the root
    | otherwise = case mapMaybe (findPath target) children of
        [] -> Nothing -- Target not found in any child subtree
        (path:_) -> Just (label : path) -- Prepend current label to the found path

-- Find the path between two nodes 'from' and 'to' in the tree.
-- Returns Nothing if either 'from' or 'to' is not in the tree,
-- or if 'from' and 'to' are the same node (implicitly handled by findPath).
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
    povTree <- fromPOV from tree -- Re-orient the tree from 'from's perspective
    findPath to povTree         -- Find the path from the new root ('from') to 'to'

