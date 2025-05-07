module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), subForest)
import Data.Maybe (mapMaybe)

-- Let's implement a helper that returns the path from root to x as a list of nodes:
pathTo :: Eq a => a -> Tree a -> Maybe [Tree a]
pathTo x t@(Node label children)
  | label == x = Just [t]
  | otherwise = case mapMaybe (pathTo x) children of
      (p:_) -> Just (t:p)
      [] -> Nothing

-- Reparent the tree at x by reversing the path from root to x
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x t = do
  p <- pathTo x t
  reparentPath p

-- Given a path from root to x, reparent the tree at x
reparentPath :: Eq a => [Tree a] -> Maybe (Tree a)
reparentPath [] = Nothing
reparentPath [t] = Just t
reparentPath (parent:child:rest) = do
  newChild <- reparentPath (child:rest)
  let Node pl pcs = parent
      Node cl ccs = newChild
      -- Remove child from parent's children
      newParentChildren = filter (/= newChild) pcs
      -- Add parent as child of child
      newChildChildren = ccs ++ [Node pl newParentChildren]
  return $ Node cl newChildChildren

-- tracePathBetween from to tree: find path from 'from' to 'to' in the tree
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  reoriented <- fromPOV from tree
  pathToNode to reoriented

-- Find path from root to node with label x, returning list of labels
pathToNode :: Eq a => a -> Tree a -> Maybe [a]
pathToNode x (Node label children)
  | label == x = Just [label]
  | otherwise = case mapMaybe (pathToNode x) children of
      (p:_) -> Just (label : p)
      [] -> Nothing
