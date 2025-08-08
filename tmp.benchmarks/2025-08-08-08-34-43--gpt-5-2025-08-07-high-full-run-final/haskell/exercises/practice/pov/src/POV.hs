module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree (..))

-- Re-root the tree at the node equal to x, if present.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree =
  case findPath x tree of
    Nothing -> Nothing
    Just (target, crumbs) -> Just (attachChain target crumbs)

-- Compute the path (inclusive) between two nodes if both are present.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  rerooted <- fromPOV from tree
  pathTo to rerooted

-- Internal helpers

-- A breadcrumb recording the parent label and the siblings around the chosen child.
data Crumb a = Crumb a [Tree a] [Tree a]

-- Find the target node and the list of crumbs from the target up to the root.
findPath :: Eq a => a -> Tree a -> Maybe (Tree a, [Crumb a])
findPath x t@(Node r cs)
  | r == x = Just (t, [])
  | otherwise = search [] cs
  where
    -- search accumulates left siblings in reverse for efficiency
    search :: [Tree a] -> [Tree a] -> Maybe (Tree a, [Crumb a])
    search _ [] = Nothing
    search leftRev (c:rs) =
      case findPath x c of
        Just (n, crumbs) -> Just (n, Crumb r (reverse leftRev) rs : crumbs)
        Nothing          -> search (c:leftRev) rs

-- Attach the ancestor chain (built from crumbs) as an extra child of the target node.
attachChain :: Tree a -> [Crumb a] -> Tree a
attachChain t [] = t
attachChain (Node rl sf) (c:cs) = Node rl (sf ++ [buildChain c cs])

-- Build the chain of ancestors as a subtree: parent -> grandparent -> ...,
-- excluding the child along the path (which becomes the parent in the re-rooted tree).
buildChain :: Crumb a -> [Crumb a] -> Tree a
buildChain (Crumb p ls rs) [] = Node p (ls ++ rs)
buildChain (Crumb p ls rs) (next:rest) =
  Node p ((ls ++ rs) ++ [buildChain next rest])

-- Path from the root of a tree to the target label (inclusive).
pathTo :: Eq a => a -> Tree a -> Maybe [a]
pathTo x (Node r cs)
  | x == r = Just [r]
  | otherwise =
      case firstJust (map (pathTo x) cs) of
        Nothing -> Nothing
        Just ps -> Just (r : ps)

-- Return the first Just value in a list or Nothing if none.
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (m:ms) = case m of
  Just _  -> m
  Nothing -> firstJust ms
