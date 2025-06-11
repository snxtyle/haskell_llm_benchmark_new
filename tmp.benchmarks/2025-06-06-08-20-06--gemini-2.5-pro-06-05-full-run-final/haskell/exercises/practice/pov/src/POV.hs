module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (listToMaybe, mapMaybe)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree =
  case findAndBuildPath x tree of
    Nothing -> Nothing
    Just (t, []) -> Just t
    Just (t@(Node label children), path) ->
      Just $ Node label (children ++ [makeBranch (reverse path)])
  where
    findAndBuildPath :: Eq a => a -> Tree a -> Maybe (Tree a, [(a, [Tree a])])
    findAndBuildPath target t@(Node tLabel tChildren)
      | tLabel == target = Just (t, [])
      | otherwise        = search tChildren []
      where
        search [] _ = Nothing
        search (c:cs) ls =
          case findAndBuildPath target c of
            Just (found, p) -> Just (found, (tLabel, reverse ls ++ cs) : p)
            Nothing         -> search cs (c:ls)

    makeBranch :: [(a, [Tree a])] -> Tree a
    makeBranch [(l, s)] = Node l s
    makeBranch ((l, s):ps) = Node l (s ++ [makeBranch ps])

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  newTree <- fromPOV from tree
  findPathTo to newTree
  where
    findPathTo :: Eq a => a -> Tree a -> Maybe [a]
    findPathTo target (Node label children)
      | label == target = Just [label]
      | otherwise = (label :) <$> listToMaybe (mapMaybe (findPathTo target) children)
