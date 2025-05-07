module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (mapMaybe, listToMaybe)

-- Find the path from the root to the node with value x
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath x (Node v children)
  | x == v    = Just [v]
  | otherwise = case mapMaybe (findPath x) children of
                  []     -> Nothing
                  (p:_)  -> Just (v : p)

-- Find the path from the root to the node with value x, returning the nodes themselves
findNodePath :: Eq a => a -> Tree a -> Maybe [Tree a]
findNodePath x n@(Node v children)
  | x == v    = Just [n]
  | otherwise = case mapMaybe (findNodePath x) children of
                  []    -> Nothing
                  (p:_) -> Just (n : p)

-- Remove a child node from a tree, returning the child and the tree with that child removed
extractChild :: Eq a => a -> Tree a -> Maybe (Tree a, Tree a)
extractChild x (Node v children) =
  case break (\(Node v' _) -> v' == x) children of
    (before, n@(Node v' cs):after)
      | v' == x -> Just (n, Node v (before ++ after))
    _ -> Nothing

-- Re-root the tree at the node with value x
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x t = do
  path <- findNodePath x t
  reroot path
  where
    reroot [n] = Just n
    reroot (parent@(Node pv pcs):child@(Node cv ccs):rest) = do
      -- Remove child from parent's children
      let newParent = Node pv (filter (\(Node v _) -> v /= cv) pcs)
      let newChild = Node cv (ccs ++ [newParent])
      reroot (child:rest) >>= \_ -> reroot (newChild:rest)
    reroot _ = Nothing

-- Helper to get the path between two nodes
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  newTree <- fromPOV from tree
  findPath to newTree
