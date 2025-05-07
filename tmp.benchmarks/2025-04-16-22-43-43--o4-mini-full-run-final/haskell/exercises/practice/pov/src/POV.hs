module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node))

-- Re-orient the tree so that 'x' becomes the root.
-- Returns Nothing if 'x' is not found.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x t = do
  -- find the subtree at x and collect [(parent, siblings), ...] up to the original root
  (origChildren, pathUp) <- findPath [] t
  let newRootChildren = case buildReversed pathUp of
        Just chain -> origChildren ++ [chain]
        Nothing    -> origChildren
  return (Node x newRootChildren)
  where
    findPath acc (Node lbl kids)
      | lbl == x  = Just (kids, acc)
      | otherwise = goKids kids
      where
        goKids [] = Nothing
        goKids (c:cs) =
          let siblings = filter (/= c) kids
              acc'     = acc ++ [(lbl, siblings)]
          in case findPath acc' c of
               Just res -> Just res
               Nothing  -> goKids cs

    buildReversed [] = Nothing
    buildReversed ups =
      let -- last entry is the original root
          (pn, sibN) = last ups
          base       = Node pn sibN
          -- all but the last, in reverse order
          tailUps    = reverse (init ups)
          chain      = foldl attach base tailUps
      in Just chain

    attach accNode (pLbl, sibs) = Node pLbl (sibs ++ [accNode])

-- Find the path of labels from 'from' to 'to', if any.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  reoriented <- fromPOV from tree
  findLabels reoriented
  where
    findLabels (Node lbl kids)
      | lbl == to = Just [lbl]
      | otherwise = goKids kids
      where
        goKids [] = Nothing
        goKids (c:cs) =
          case findLabels c of
            Just path -> Just (lbl : path)
            Nothing   -> goKids cs
