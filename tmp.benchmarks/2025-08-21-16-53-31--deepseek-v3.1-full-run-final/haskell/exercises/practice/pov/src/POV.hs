module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.List (find)
import Data.Maybe (fromJust, isJust, listToMaybe)

-- Find the path from root to a node
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath target (Node label children)
  | target == label = Just [label]
  | otherwise = case listToMaybe (mapMaybe (findPath target) children) of
      Just path -> Just (label : path)
      Nothing -> Nothing

-- Helper function to map with Maybe
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
  Just y -> y : mapMaybe f xs
  Nothing -> mapMaybe f xs

-- Find a node in the tree
findNode :: Eq a => a -> Tree a -> Maybe (Tree a)
findNode target (Node label children)
  | target == label = Just (Node label children)
  | otherwise = listToMaybe (mapMaybe (findNode target) children)

-- Build a tree from a new root by reversing the path
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV newRoot tree = 
  case findPath newRoot tree of
    Just path -> Just (rebuildTree path tree)
    Nothing -> Nothing
  where
    rebuildTree :: Eq a => [a] -> Tree a -> Tree a
    rebuildTree [x] _ = fromJust (findNode x tree)
    rebuildTree (x:y:xs) _ = 
      let Just (Node parentLabel children) = findNode x tree
          Just (Node childLabel childChildren) = findNode y tree
          -- Remove the child from parent's children
          newParentChildren = filter (\(Node v _) -> v /= childLabel) children
          -- Create new child with parent as one of its children
          newChild = Node childLabel (Node parentLabel newParentChildren : childChildren)
      in rebuildTree (y:xs) (replaceNode childLabel newChild tree)
    
    replaceNode :: Eq a => a -> Tree a -> Tree a -> Tree a
    replaceNode target replacement (Node label children)
      | target == label = replacement
      | otherwise = Node label (map (replaceNode target replacement) children)

-- Find path between two nodes
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  fromPath <- findPath from tree
  toPath <- findPath to tree
  
  -- Find the common prefix
  let commonPrefixLength = length $ takeWhile (uncurry (==)) $ zip fromPath toPath
      fromSuffix = drop commonPrefixLength fromPath
      toSuffix = drop commonPrefixLength toPath
      
  -- Path is: reverse of fromSuffix (to go up to common ancestor) + toSuffix (to go down to target)
  return (reverse fromSuffix ++ toSuffix)
