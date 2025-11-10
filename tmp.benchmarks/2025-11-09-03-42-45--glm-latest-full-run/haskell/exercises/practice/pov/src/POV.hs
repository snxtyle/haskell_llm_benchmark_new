module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (listToMaybe, mapMaybe)

-- | Re-orientates a tree from the point of view of a given node.
--   If the node is not in the tree, it returns Nothing.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = reorient tree x
  where
    -- Helper function to recursively find and reorient the tree.
    -- It returns the new tree if the target is found in the current subtree.
    reorient :: Eq a => Tree a -> a -> Maybe (Tree a)
    reorient (Node r children) target
      -- If the current node is the target, we are at the new root.
      | r == target = Just (Node r children)
      -- Otherwise, search for the target in the children.
      | otherwise = findInChildren children target r

    -- Helper to search through children and perform the reorientation.
    -- It takes the list of children, the target node value, and the value of the current parent.
    findInChildren :: Eq a => [Tree a] -> a -> a -> Maybe (Tree a)
    findInChildren children target parentVal = go children []
      where
        go [] _ = Nothing
        go (c:cs) acc =
          case reorient c target of
            Just (Node newX newChildren) ->
              let
                -- Siblings are the ones we've seen (acc) and the ones yet to see (cs)
                allSiblings = reverse acc ++ cs
                newParent = Node parentVal allSiblings
              in
                Just (Node newX (newChildren ++ [newParent]))
            Nothing -> go cs (c:acc)

-- | Finds the path between two nodes in a tree.
--   Returns Nothing if either node is not in the tree or if they are the same node.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree
  | from == to = Nothing
  | otherwise = do
      -- First, re-root the tree at the 'from' node.
      newTree <- fromPOV from tree
      -- Then, find the path from the new root to the 'to' node.
      findPathFromRoot to newTree
  where
    -- Helper to find a path from the root of a tree to a target node.
    findPathFromRoot :: Eq a => a -> Tree a -> Maybe [a]
    findPathFromRoot target (Node root children)
      -- If the current root is the target, the path is just the root.
      | root == target = Just [root]
      -- Otherwise, search for the target in the children.
      | otherwise = do
          -- Find the first child that contains the path to the target.
          pathInChild <- listToMaybe $ mapMaybe (\c -> findPathFromRoot target c) children
          -- Prepend the current root to the path found in the child.
          return (root : pathInChild)
