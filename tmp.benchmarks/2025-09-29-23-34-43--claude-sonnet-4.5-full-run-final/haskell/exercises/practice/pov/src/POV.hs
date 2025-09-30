module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (listToMaybe)
import Data.List (partition)

-- | Reparent a tree from the point of view of the given node
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV target tree
  | rootLabel tree == target = Just tree
  | otherwise = do
      path <- findPath target tree
      reparentFromPath path tree

-- | Find the path from root to target node
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath target (Node label children)
  | label == target = Just [target]
  | otherwise = listToMaybe
      [ label : path
      | child <- children
      , path <- maybe [] pure (findPath target child)
      ]

-- | Reparent the tree given a path from root to new root
reparentFromPath :: Eq a => [a] -> Tree a -> Maybe (Tree a)
reparentFromPath [] _ = Nothing
reparentFromPath [_] tree = Just tree  -- Already at target
reparentFromPath (current:next:rest) (Node label children) =
  case partition (containsNode next) children of
    ([targetChild], otherChildren) -> do
      -- Recursively reparent from the child's perspective
      newSubtree <- reparentFromPath (next:rest) targetChild
      -- Create a new tree with current node as child of the reparented subtree
      let currentAsChild = Node label otherChildren
      return $ insertParent currentAsChild newSubtree
    _ -> Nothing

-- | Check if a tree contains a node with given label
containsNode :: Eq a => a -> Tree a -> Bool
containsNode target (Node label children) =
  label == target || any (containsNode target) children

-- | Insert a parent node into the children of a tree
insertParent :: Tree a -> Tree a -> Tree a
insertParent parent (Node label children) =
  Node label (parent : children)

-- | Trace the path between two nodes in a tree
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  reparented <- fromPOV from tree
  pathFromRoot <- findPath to reparented
  return pathFromRoot
