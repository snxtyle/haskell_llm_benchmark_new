module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), Forest, rootLabel)
import Data.List (delete)

-- | Reparent a tree with the specified node as the new root
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV target tree = case findPathToNode target tree of
  Nothing -> Nothing
  Just path -> Just $ reparentTree path tree

-- | Find the path between two nodes in the tree
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  fromTree <- fromPOV from tree
  path <- findPathToNode to fromTree
  return path

-- | Find the path from root to target node (including both)
findPathToNode :: Eq a => a -> Tree a -> Maybe [a]
findPathToNode target (Node label children)
  | target == label = Just [label]
  | otherwise = case firstJust (map (findPathToNode target) children) of
      Nothing -> Nothing
      Just path -> Just (label : path)

-- | Helper to get the first Just value from a list of Maybes
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x:_) = Just x

-- | Reparent the tree based on the path from old root to new root
reparentTree :: Eq a => [a] -> Tree a -> Tree a
reparentTree [target] (Node label children) = Node target children
reparentTree (current:next:rest) (Node label children) = 
  let targetChild = findChild next children
      otherChildren = delete targetChild children
      -- Create a new parent node with current as root and other children
      newParent = Node current otherChildren
      -- Recursively reparent the target subtree
      reparentedSubtree = reparentTree (next:rest) targetChild
      -- Add the new parent as a child to the reparented subtree
  in addParentToTree newParent reparentedSubtree

-- | Find a child with specific root label
findChild :: Eq a => a -> Forest a -> Tree a
findChild target children = 
  head [child | child <- children, rootLabel child == target]

-- | Add a parent node to a tree (making the tree a child of the parent)
addParentToTree :: Tree a -> Tree a -> Tree a
addParentToTree parent (Node label children) = 
  Node label (parent : children)
