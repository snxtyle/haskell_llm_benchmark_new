module Dominoes (chain) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Represents a domino stone with two numbers.
type Domino = (Int, Int)

-- | Computes a chain of dominoes.
-- A chain is valid if adjacent dominoes match ends,
-- all dominoes are used, and the first and last numbers match.
chain :: [Domino] -> Maybe [Domino]
chain [] = Just [] -- An empty list of dominoes forms a valid empty chain.
chain dominoes =
  -- First, check the necessary condition for an Eulerian circuit:
  -- Every number must appear an even number of times across all domino halves.
  if not (hasEulerianProperty dominoes)
  then Nothing
  else
    -- If the property holds, try to build a chain using backtracking.
    -- We start the chain with the first domino and try to extend it.
    -- The search implicitly checks for connectivity. If the graph is disconnected,
    -- the search will fail to use all dominoes.
    case dominoes of
      [] -> Just [] -- Already handled by the first line, but safe pattern matching
      (firstDomino : remainingDominoes) ->
          -- Start the recursive backtracking search
          buildChain remainingDominoes [firstDomino]

-- | Checks if the set of dominoes satisfies the Eulerian property necessary
-- for a closed chain (circuit): each number must appear an even number of times.
hasEulerianProperty :: [Domino] -> Bool
hasEulerianProperty dominoes =
  let -- Count occurrences of each number on domino halves
      counts = foldr countEnds Map.empty dominoes
      -- Helper function to update counts for a single domino
      countEnds :: Domino -> Map Int Int -> Map Int Int
      countEnds (a, b) acc = Map.insertWith (+) a 1 $ Map.insertWith (+) b 1 acc
  in -- Check if all counts are even
     Map.null $ Map.filter odd counts

-- | Recursive helper function to build the domino chain using backtracking.
-- Parameters:
--   remaining: The list of dominoes yet to be placed in the chain.
--   currentChain: The chain built so far, ordered correctly.
buildChain :: [Domino] -> [Domino] -> Maybe [Domino]
buildChain [] currentChain =
  -- Base case: All dominoes have been used.
  -- Check if the chain is circular (first number matches last number).
  case currentChain of
    -- This case should not be reached if the initial list was non-empty.
    [] -> Just []
    -- Get the first number of the first domino and the second number of the last domino.
    ((firstNum, _) : _) ->
      let (_, lastNum) = last currentChain -- `last` is safe here because currentChain is non-empty
      in if firstNum == lastNum
         then Just currentChain -- Valid circular chain found.
         else Nothing          -- Chain formed, but not circular. Backtrack.

buildChain remaining currentChain =
  -- Recursive step: Try to extend the chain.
  let -- Get the number on the open end of the current chain.
      (_, currentEndNum) = last currentChain -- `last` is safe here as currentChain is non-empty
      -- Find the first domino from 'remaining' that can extend the chain.
      -- We pass `remaining` (available dominoes) and `[]` (initially empty list of tried dominoes).
  in findExtension remaining []
  where
    -- Helper function to iterate through 'remaining' dominoes, trying each one.
    -- Parameters:
    --   available: Dominoes still available to try from the original 'remaining' list.
    --   tried: Dominoes from the original 'remaining' list that have already been tried
    --          at this step of the recursion but did not lead to a solution.
    findExtension :: [Domino] -> [Domino] -> Maybe [Domino]
    findExtension [] _ = Nothing -- No more available dominoes can extend the chain from this state.
    findExtension (d@(x, y) : ds) tried =
      -- The remaining dominoes for the recursive call if 'd' is used.
      -- This consists of the dominoes in 'tried' and the rest of the 'available' list 'ds'.
      let remaining' = tried ++ ds
      in -- Try matching the first half 'x' of the domino 'd' with the current end number.
         if x == currentEndNum then
           -- If 'x' matches, recursively call buildChain with 'd' appended to the chain.
           case buildChain remaining' (currentChain ++ [d]) of
             Just solution -> Just solution -- Solution found down this path. Return it up the call stack.
             Nothing -> tryFlipOrContinue d ds tried -- 'd' didn't lead to a solution, backtrack: try flipping 'd' or the next domino.
         else
           -- First half 'x' didn't match, try flipping 'd' or the next domino.
           tryFlipOrContinue d ds tried

    -- Helper function to try flipping the domino 'd' or move to the next available domino 'ds'.
    tryFlipOrContinue :: Domino -> [Domino] -> [Domino] -> Maybe [Domino]
    tryFlipOrContinue d@(x, y) ds tried =
      let remaining' = tried ++ ds
      in -- Try matching the second half 'y' of the domino 'd' (requires flipping the domino).
         -- We add 'x /= y' condition to avoid redundant checks for doubles like (3, 3),
         -- as they would have been handled by the 'if x == currentEndNum' check already if x == y.
         if y == currentEndNum && x /= y then
           -- If 'y' matches, recursively call buildChain with the flipped domino '(y, x)' appended.
           case buildChain remaining' (currentChain ++ [(y, x)]) of
             Just solution -> Just solution -- Solution found down this path. Return it.
             Nothing -> findExtension ds (d : tried) -- Flipped 'd' didn't lead to a solution. Backtrack: try the next available domino 'ds'. Add 'd' to the 'tried' list.
         else
           -- Neither orientation of 'd' worked, or 'd' was a double and didn't match.
           -- Try the next available domino 'ds'. Add 'd' to the 'tried' list.
           findExtension ds (d : tried)
