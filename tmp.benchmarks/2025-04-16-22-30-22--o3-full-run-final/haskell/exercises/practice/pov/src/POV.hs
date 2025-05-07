module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree (..))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (foldl')

-- Public API -------------------------------------------------------------

-- | Re‑root a tree so that the node whose label is @x@ becomes the root of the
--   returned tree.  Fails with 'Nothing' when @x@ is not present.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x t = do
  p <- pathTo x t               -- path from current root to @x@
  pure (reRootByPath p t)       -- re‑orient the tree along that path

-- | Compute the labels encountered when walking from the node labelled @from@
--   to the node labelled @to@, inclusive of both endpoints.
--   Returns 'Nothing' when one (or both) of the labels are absent.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to t
  | from == to = do
      present <- pathTo from t
      pure [from | not (null present)]          -- singleton path if node exists
  | otherwise = do
      pathFrom <- pathTo from t                 -- root → from
      pathTo'  <- pathTo to   t                 -- root → to
      let (common, suffixF, suffixT) = splitOnDiverge pathFrom pathTo'
      case common of
        []      -> Nothing                      -- disjoint — should not happen in a tree
        lca : _ -> let upward   = reverse suffixF
                       downward = suffixT
                   in pure (upward ++ [lca] ++ downward)

-- Internal helpers -------------------------------------------------------

-- | Find a path from the root of the tree to the node whose label is @x@.
--   The path is expressed as the list of node labels encountered,
--   starting with the current root and finishing with @x@.
pathTo :: Eq a => a -> Tree a -> Maybe [a]
pathTo x (Node lbl cs)
  | x == lbl  = Just [lbl]
  | otherwise = listToMaybe $ mapMaybe prepend (map (pathTo x) cs)
  where
    prepend Nothing  = Nothing
    prepend (Just p) = Just (lbl : p)

-- | Re‑root the tree so that the last element of the supplied path
--   (which must describe a path from the current root to the desired root)
--   becomes the new root.
reRootByPath :: Eq a => [a] -> Tree a -> Tree a
reRootByPath [ _ ] t = t                                        -- already rooted
reRootByPath (parent : rest@(child : _)) (Node lbl cs)
  | lbl /= parent = error "reRootByPath: path does not match tree structure"
  | otherwise     =
      case extractChild child cs of
        Nothing                 -> Node lbl cs                  -- Should not happen
        Just (target, siblings) ->
          let newChild   = reRootByPath rest target
              newParent  = Node lbl siblings
          in attachUnder child newChild newParent
reRootByPath _ _ = error "reRootByPath: empty path supplied"

-- | Attach the third tree as an additional child of the node whose label
--   matches @targetLabel@ within the supplied tree.  This searches the tree
--   depth‑first; if the label is not found, the original tree is returned
--   unchanged (this should not happen if the data are consistent).
attachUnder :: Eq a => a -> Tree a -> Tree a -> Tree a
attachUnder targetLabel (Node lbl cs) newChild
  | lbl == targetLabel = Node lbl (cs ++ [newChild])
  | otherwise          = Node lbl (map (\c -> attachUnder targetLabel c newChild) cs)

-- | From a list of children, remove and return the one whose root label matches
--   the supplied label.  Returns 'Nothing' if no such child exists.
extractChild :: Eq a => a -> [Tree a] -> Maybe (Tree a, [Tree a])
extractChild _ [] = Nothing
extractChild x (t@(Node lbl _) : ts)
  | x == lbl  = Just (t, ts)
  | otherwise = do
      (found, rest) <- extractChild x ts
      pure (found, t : rest)

-- | Given two root‑to‑node paths, split them into their common prefix (returned
--   reversed, so its head is the lowest common ancestor) and the remaining
--   suffixes after the paths diverge.
splitOnDiverge :: Eq a => [a] -> [a] -> ([a], [a], [a])
splitOnDiverge = go []
  where
    go acc (x:xs) (y:ys)
      | x == y    = go (x:acc) xs ys
    go acc xs ys  = (acc, xs, ys)
    go acc [] ys  = (acc, [], ys)      -- shouldn't happen in a tree
    go acc xs []  = (acc, xs, [])      -- likewise
