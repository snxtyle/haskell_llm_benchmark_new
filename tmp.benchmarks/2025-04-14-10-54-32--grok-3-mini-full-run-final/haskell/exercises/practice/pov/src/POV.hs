module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (listToMaybe, maybeToList)

-- Helper function to get the path from the root to a target node
getPath :: Eq a => a -> Tree a -> Maybe [a]
getPath target tree = go [tree] []
  where
    go [] acc = Nothing  -- No path found
    go (Node v subtrees : rest) acc
      | v == target = Just (reverse (v : acc))  -- Found the target
      | otherwise = go (rest ++ subtrees) (v : acc)  -- Continue searching

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = 
  -- Basic implementation: Return the subtree rooted at x, if it exists.
  -- This does not fully reorient the tree as described; it only extracts the subtree.
  -- For a full implementation, we would need to rebuild the tree with the path inverted and subtrees adjusted.
  let findSubtree (Node v subtrees) = if v == x then Just (Node v subtrees) else listToMaybe (concatMap (maybeToList . findSubtree) subtrees)
  in findSubtree tree

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween fromNode toNode tree = do
  pathFromRootToFrom <- getPath fromNode tree  -- Path from root to 'fromNode'
  pathFromRootToTo <- getPath toNode tree      -- Path from root to 'toNode'
  let commonPrefix = takeWhile id $ zipWith (==) pathFromRootToFrom pathFromRootToTo  -- Find common prefix
      commonLength = length commonPrefix
      pathToCommon = drop commonLength pathFromRootToFrom  -- From 'fromNode' to common ancestor (reversed)
      pathFromCommon = drop commonLength pathFromRootToTo  -- From common ancestor to 'toNode'
  return (reverse pathToCommon ++ pathFromCommon)  -- Combine: from 'fromNode' to common, then to 'toNode'
