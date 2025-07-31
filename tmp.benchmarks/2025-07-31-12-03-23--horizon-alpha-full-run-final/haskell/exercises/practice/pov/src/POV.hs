module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))

-- Re-root the tree at the node with label x, if present.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x t = reroot x t

-- Produce the path of labels from 'from' to 'to', if both exist in the tree.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to t = do
  t' <- fromPOV from t
  findPath to t'

-- Helpers

-- Find the path (as list of labels) from current root to the target label.
-- Returns Just [rootLabel, ..., targetLabel] or Nothing if not found.
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath target (Node v cs)
  | v == target = Just [v]
  | otherwise = case firstJust (map (findPath target) cs) of
      Nothing   -> Nothing
      Just path -> Just (v : path)

-- Reroot the tree at the node with the given label, if it exists.
-- This works by finding the path from the current root to the target and
-- flipping edges along that path while preserving other subtrees.
reroot :: Eq a => a -> Tree a -> Maybe (Tree a)
reroot target t = do
  path <- findPath target t
  buildReoriented t path
  where
    -- Build a reoriented tree given the original tree and the path of labels
    -- from root to the desired new root.
    buildReoriented :: Eq a => Tree a -> [a] -> Maybe (Tree a)
    buildReoriented _ [] = Nothing
    buildReoriented orig@(Node v _) (p:ps)
      | v /= p    = Nothing
      | otherwise = reorient orig ps

    -- Reorient starting at the current node, following the remaining labels
    -- in the path to the target. When we reach the target, make it the root.
    reorient :: Eq a => Tree a -> [a] -> Maybe (Tree a)
    reorient (Node v cs) [] = Just (Node v cs) -- already at target; children unchanged
    reorient (Node v cs) (nxt:rest) = do
      -- split children into the one along the path and the others
      (along, others) <- takeChild nxt cs
      reorientedAlong <- reorient along rest
      -- After reorienting the subtree 'along' so that its root is the target,
      -- attach the current node 'v' under that reoriented subtree, excluding the edge back.
      let vAsChild = Node v others
      pure (attachAsChild vAsChild reorientedAlong)

    -- Find and remove the child with the given root label.
    takeChild :: Eq a => a -> [Tree a] -> Maybe (Tree a, [Tree a])
    takeChild _ [] = Nothing
    takeChild k (t0@(Node v _):ts)
      | k == v = Just (t0, ts)
      | otherwise = do
          (found, rest) <- takeChild k ts
          pure (found, t0 : rest)

    -- Attach a node as a child of the root of the given tree.
    attachAsChild :: Tree a -> Tree a -> Tree a
    attachAsChild child (Node r rs) = Node r (rs ++ [child])

-- Return the first Just value from a list, or Nothing if all are Nothing.
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs) = case x of
  Just _  -> x
  Nothing -> firstJust xs
