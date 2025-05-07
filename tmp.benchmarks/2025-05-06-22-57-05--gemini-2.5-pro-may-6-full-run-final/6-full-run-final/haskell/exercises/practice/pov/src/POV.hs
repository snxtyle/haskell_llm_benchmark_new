module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node))

-- fromPOV re-orients the tree to be from the POV of the target node.
-- If the target node is not in the tree, it returns Nothing.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV target tree = findAndReparent target [] tree
  where
    -- findAndReparent attempts to find the target node and restructure the tree.
    -- Parameters:
    --   t: The target node's label we're looking for.
    --   accParents: A list of trees representing the re-parented ancestors encountered
    --               on the path from the original root down to currentTree's original parent.
    --               These will become children of 't' if 't' is currentTree or in its subForest.
    --   currentTree: The current subtree being processed.
    findAndReparent :: Eq a => a -> [Tree a] -> Tree a -> Maybe (Tree a)
    findAndReparent t accParents currentTree@(Node val forest)
      -- If the current node is the target:
      -- Its new children are its original children plus the accumulated re-parented ancestors.
      | val == t  = Just (Node t (forest ++ accParents))
      -- Otherwise, search for the target in the children of the current node.
      | otherwise = searchInChildren forest []
      where
        -- searchInChildren iterates through the children of currentTree.
        -- Parameters:
        --   childrenToSearch: The list of children of currentTree yet to be searched.
        --   processedSiblings: Children of currentTree already searched (or to the "left" of current child).
        -- No explicit signature for searchInChildren:
        searchInChildren [] _ = Nothing -- Target not found in any child.
        searchInChildren (childToCheck : remainingSiblings) processedSiblings =
          -- Construct the new "parent" node that currentTree's root (val) would become
          -- if the target is found in childToCheck's branch.
          -- Its children would be its other original children (currentSiblingsOfChild)
          -- and its already re-parented original ancestors (accParents).
          let currentSiblingsOfChild = processedSiblings ++ remainingSiblings
              newParentAsChild = Node val (currentSiblingsOfChild ++ accParents)
          in case findAndReparent t [newParentAsChild] childToCheck of
               Just foundTree -> Just foundTree -- Target found in this branch.
               Nothing        -> searchInChildren remainingSiblings (childToCheck : processedSiblings) -- Check next sibling.

-- tracePathBetween finds the path between two nodes in a tree.
-- If either node is not in the tree, or if there's no path (which shouldn't happen in a valid tree),
-- it returns Nothing.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween startNode endNode tree =
  case fromPOV startNode tree of
    Nothing      -> Nothing -- startNode was not found in the tree.
    Just povTree -> findPathFromRoot endNode povTree
  where
    -- findPathFromRoot searches for a target node in a tree (assuming tree's root is the start of the path).
    -- Returns Just pathList if found, Nothing otherwise.
    findPathFromRoot :: Eq a => a -> Tree a -> Maybe [a]
    findPathFromRoot target (Node val forest)
      -- If the current node is the target, path is just [val].
      | val == target = Just [val]
      -- Otherwise, search in the subForest.
      | otherwise     = searchInSubForest forest
      where
        -- searchInSubForest iterates through children to find the target.
        -- No explicit signature for searchInSubForest:
        searchInSubForest [] = Nothing -- Target not found in this branch.
        searchInSubForest (child : cs) =
          case findPathFromRoot target child of
            Just pathSoFar -> Just (val : pathSoFar) -- Prepend current node to path from child.
            Nothing        -> searchInSubForest cs   -- Check next child.
