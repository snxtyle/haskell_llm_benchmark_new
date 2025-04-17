module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree (..), rootLabel, subForest)

-- | Try to find the path (as a list of nodes) from the root of the tree to the
--   node whose label equals the given value.  The resulting list is ordered
--   from the current root down to the sought‑for node.
findPath :: Eq a => a -> Tree a -> Maybe [Tree a]
findPath target n@(Node lbl children)
  | target == lbl = Just [n]
  | otherwise     = firstJust (map (findPath target) children) >>= \p -> Just (n : p)
  where
    -- Return the first 'Just' in a list, if any.
    firstJust :: [Maybe b] -> Maybe b
    firstJust []             = Nothing
    firstJust (mx : mxs) =
      case mx of
        Just _  -> mx
        Nothing -> firstJust mxs

-- | Re‑orient a tree so that the first element in the given list becomes the
--   new root.  The list must contain the nodes that lie on the path from the
--   original root (last element) to the desired new root (first element).
rebuild :: Eq a => [Tree a] -> Tree a
rebuild []               = error "rebuild: empty path supplied"
rebuild (current : rest) = go current rest
  where
    -- The helper carries the (partially rebuilt) child and the remaining
    -- ancestors.  At each step we:
    --   1. Remove the child we just came from from the current parent's
    --      sub‑forest (to avoid cycles),
    --   2. Attach that trimmed parent as a new child of the current node,
    --   3. Recurse upwards.
    go curr [] = curr
    go curr (parent : ancestors) =
      let childLabel     = rootLabel curr
          prunedSiblings = filter (\t -> rootLabel t /= childLabel) (subForest parent)
          parentTrimmed  = Node (rootLabel parent) prunedSiblings
          newCurr        = Node (rootLabel curr)
                                (subForest curr ++ [go parentTrimmed ancestors])
      in newCurr

-- | Re‑root the tree so that the supplied node becomes the new root.  If the
--   node is not present in the tree, returns 'Nothing'.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = do
  path <- findPath x tree          -- path from original root to x
  let pathReversed = reverse path  -- make x the first element
  pure (rebuild pathReversed)

-- | Trace the path between two nodes.  The resulting list starts with the
--   label of the @from@ node and ends with the label of the @to@ node.
--   Returns 'Nothing' if either node is not present in the tree.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  reRooted <- fromPOV from tree
  path     <- findPath to reRooted
  pure (map rootLabel path)
