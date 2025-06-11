module POV (fromPOV, tracePathBetween) where

import Data.Maybe (mapMaybe, maybeToList)
import Data.Tree  (Tree (..))

-- | Produce a new tree whose root is the requested node.
--   The relative ordering of every other node is preserved,
--   only the direction of the links along the path to the
--   requested node is reversed.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x t = do
    path <- findPath x t                     -- root-to-x inclusive
    pure (reparent path t)

-- | Return the list of node labels along the unique path that
--   connects the two requested nodes (both end-points included).
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to t = do
    povTree <- fromPOV from t                -- re-root at the start node
    findPath to povTree                      -- path becomes root-to-to


------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------

-- | Find the path (as labels) from the current root to the
--   requested node.  When the node is not present, produce Nothing.
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath goal (Node lbl children)
    | goal == lbl = Just [lbl]
    | otherwise   =
        let attempt = mapMaybe (findPath goal) children
        in case attempt of
             (p:_) -> Just (lbl : p)         -- use first successful path
             []    -> Nothing

-- | Re-root the tree so that the node that appears last in the
--   provided path (which must be the original path from the old
--   root to the new one) becomes the new root.
--
--   pre-condition:  (head path) == rootLabel tree
--                   path non-empty
reparent :: Eq a => [a] -> Tree a -> Tree a
reparent path t = go path t Nothing
  where
    -- The accumulator carries the part of the tree that has already
    -- been detached and flipped (i.e. the section closer to the old
    -- root).  At each step we detach the next node along the path,
    -- make it a parent of the accumulated piece and continue.
    go :: Eq a => [a] -> Tree a -> Maybe (Tree a) -> Tree a
    go [] _ _ = error "reparent: empty path"
    -- Last element of the path -> this becomes the new root.
    go [_] (Node lbl children) acc =
        Node lbl (children ++ maybeToList acc)
    -- Still moving down the path.
    go (p:ps@(c:_)) (Node lbl children) acc
        | p /= lbl  = error "reparent: path does not match tree"
        | otherwise =
            case splitChild c children of
              Nothing -> error "reparent: path invalid – child missing"
              Just (childSub, others) ->
                  let newAcc = Just (Node lbl (others ++ maybeToList acc))
                  in go ps childSub newAcc

-- | Split the list of children into (the one with a given label, rest).
splitChild :: Eq a => a -> [Tree a] -> Maybe (Tree a, [Tree a])
splitChild lbl = go []
  where
    go _   [] = Nothing
    go acc (t@(Node l _):ts)
        | l == lbl = Just (t, reverse acc ++ ts)
        | otherwise = go (t:acc) ts
