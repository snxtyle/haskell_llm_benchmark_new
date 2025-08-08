module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (maybeToList)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = reparent <$> pathTo x tree

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  rooted <- fromPOV from tree
  labels <$> pathTo to rooted
  where
    labels :: [Tree a] -> [a]
    labels = map rootLabel

-- Find the path (as a list of subtrees) from the root to the node with label x.
-- The returned list includes both the root and the target node.
pathTo :: Eq a => a -> Tree a -> Maybe [Tree a]
pathTo x n@(Node l cs)
  | l == x    = Just [n]
  | otherwise = case firstJust (map (pathTo x) cs) of
      Just p  -> Just (n : p)
      Nothing -> Nothing

-- Re-parent the tree such that the last element of the path becomes the new root.
-- The parent at each step is appended as the last child to preserve sibling order.
reparent :: Eq a => [Tree a] -> Tree a
reparent = go Nothing
  where
    go :: Eq a => Maybe (Tree a) -> [Tree a] -> Tree a
    go parent [Node l cs] =
      Node l (cs ++ maybeToList parent)
    go parent (Node l cs : next@(Node nl _) : rest) =
      let cs'   = removeLabel nl cs
          this' = Node l (cs' ++ maybeToList parent)
      in go (Just this') (next : rest)
    go _ [] = error "reparent: empty path (internal error)"

-- Remove the first child whose rootLabel equals the given label.
removeLabel :: Eq a => a -> [Tree a] -> [Tree a]
removeLabel _ [] = []
removeLabel x (t:ts)
  | rootLabel t == x = ts
  | otherwise        = t : removeLabel x ts

-- Get the first Just value from a list, if any.
firstJust :: [Maybe a] -> Maybe a
firstJust []             = Nothing
firstJust (Just y : _ )  = Just y
firstJust (Nothing : xs) = firstJust xs
