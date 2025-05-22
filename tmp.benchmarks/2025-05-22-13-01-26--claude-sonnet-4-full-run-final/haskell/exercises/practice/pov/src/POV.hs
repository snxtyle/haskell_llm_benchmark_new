module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), rootLabel, subForest)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV target tree
  | rootLabel tree == target = Just tree
  | otherwise = case findAndReparent target tree of
      Just newTree -> Just newTree
      Nothing -> Nothing

findAndReparent :: Eq a => a -> Tree a -> Maybe (Tree a)
findAndReparent target tree = go [] tree
  where
    go ancestors (Node label children)
      | label == target = Just $ buildNewTree target ancestors children
      | otherwise = case findInChildren ancestors (Node label children) children of
          Just result -> Just result
          Nothing -> Nothing
    
    findInChildren ancestors parent [] = Nothing
    findInChildren ancestors parent (child:rest) =
      case go (parent:ancestors) child of
        Just result -> Just result
        Nothing -> findInChildren ancestors parent rest

buildNewTree :: Eq a => a -> [Tree a] -> [Tree a] -> Tree a
buildNewTree target ancestors children =
  Node target (children ++ ancestorTrees)
  where
    ancestorTrees = case ancestors of
      [] -> []
      (Node parentLabel parentChildren : restAncestors) ->
        let siblingsOfTarget = filter (not . containsInSubtree target) parentChildren
            newParent = buildNewTree parentLabel restAncestors siblingsOfTarget
        in [newParent]

containsInSubtree :: Eq a => a -> Tree a -> Bool
containsInSubtree target (Node label children) =
  label == target || any (containsInSubtree target) children

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  reparentedTree <- fromPOV from tree
  path <- findPathFromRoot to reparentedTree
  return path

findPathFromRoot :: Eq a => a -> Tree a -> Maybe [a]
findPathFromRoot target tree = go [rootLabel tree] tree
  where
    go path (Node label children)
      | label == target = Just path
      | otherwise = findInSubtrees path children
    
    findInSubtrees path [] = Nothing
    findInSubtrees path (child:rest) =
      case go (path ++ [rootLabel child]) child of
        Just result -> Just result
        Nothing -> findInSubtrees path rest
