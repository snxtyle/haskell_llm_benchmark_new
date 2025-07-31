module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.List (find)
import Data.Maybe (mapMaybe, isJust, fromJust)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV target tree = 
  case findPathToRoot target tree [] of
    Nothing -> Nothing
    Just path -> Just $ rebuildFromPath (reverse path)
  where
    findPathToRoot :: Eq a => a -> Tree a -> [Tree a] -> Maybe [Tree a]
    findPathToRoot x t ancestors
      | rootLabel t == x = Just (t : ancestors)
      | null (subForest t) = Nothing
      | otherwise = 
        let results = mapMaybe (\child -> findPathToRoot x child (t { subForest = filter (/= child) (subForest t) } : ancestors)) (subForest t)
        in case results of
          [] -> Nothing
          (p:_) -> Just p
    
    rebuildFromPath :: [Tree a] -> Tree a
    rebuildFromPath [] = error "Empty path"
    rebuildFromPath [t] = t
    rebuildFromPath (t:ts) = 
      let parent = head ts
          rest = tail ts
          newSubForest = t : filter (\c -> rootLabel c /= rootLabel t) (subForest parent)
          newParent = parent { subForest = newSubForest }
      in rebuildFromPath (newParent : rest)

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = 
  case fromPOV from tree of
    Nothing -> Nothing
    Just reorientedTree -> tracePath to reorientedTree
  where
    tracePath :: Eq a => a -> Tree a -> Maybe [a]
    tracePath target (Node label children)
      | label == target = Just [label]
      | otherwise = 
        case filter isJust $ map (tracePath target) children of
          [] -> Nothing
          (Just path:_) -> Just (label : path)
