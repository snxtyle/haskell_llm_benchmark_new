module TwoBucket (measure) where

import Data.Set (Set)
import qualified Data.Set as Set

-- Type aliases for clarity
type State = (Int, Int)
type Capacity = (Int, Int)
type Moves = Int

-- | Determines the number of actions to measure a specific amount of water.
-- Takes the capacities of the two buckets and the target amount.
-- Assumes the first bucket is the one to be filled first.
-- Returns Nothing if no solution is possible.
-- Otherwise, returns Just (number_of_moves, (final_state_bucket1, final_state_bucket2)).
measure :: Capacity -> Int -> Maybe (Moves, State)
measure (cap1, cap2) target
  -- A solution is impossible if the target is larger than both buckets
  -- or if the target is not a multiple of the GCD of the capacities.
  | target > max cap1 cap2 = Nothing
  | target `mod` gcd cap1 cap2 /= 0 = Nothing
  | otherwise = bfs initialQueue initialVisited
  where
    -- The problem states we always start by filling bucket 1.
    -- The first action is filling bucket 1.
    initialState :: State
    initialState = (0, 0)
    
    firstState :: State
    firstState = (cap1, 0)
    
    initialQueue :: [(State, Moves)]
    initialQueue = [(firstState, 1)] -- (state, moves)
    
    initialVisited :: Set State
    initialVisited = Set.fromList [initialState, firstState]

    -- Breadth-First Search implementation to find the shortest path.
    bfs :: [(State, Moves)] -> Set State -> Maybe (Moves, State)
    bfs [] _ = Nothing -- Queue is empty, no solution found
    bfs ((state@(x, y), moves) : restOfQueue) visited =
      -- Check if the current state is a goal state.
      if x == target || y == target
        then Just (moves, state)
        else
          -- Generate all possible next states from the current state.
          let nextStates = generateNextStates state (cap1, cap2)
              -- Filter out states that have already been visited or are forbidden.
              validNewStates = filter (\s -> not (Set.member s visited) && not (isForbidden s)) nextStates
              -- Add new states to the queue with an incremented move count.
              newQueue = restOfQueue ++ zip validNewStates (repeat (moves + 1))
              -- Add new states to the set of visited states.
              newVisited = Set.union visited (Set.fromList validNewStates)
          in bfs newQueue newVisited

    -- Checks if a state is forbidden: starting bucket (1) is empty, other (2) is full.
    isForbidden :: State -> Bool
    isForbidden (0, y) = y == cap2
    isForbidden _ = False

    -- Generates all 6 possible next states from a given state.
    generateNextStates :: State -> Capacity -> [State]
    generateNextStates (x, y) (c1, c2) =
      let -- 1. Fill bucket 1
          fill1 = (c1, y)
          -- 2. Fill bucket 2
          fill2 = (x, c2)
          -- 3. Empty bucket 1
          empty1 = (0, y)
          -- 4. Empty bucket 2
          empty2 = (x, 0)
          -- 5. Pour from bucket 1 to bucket 2
          pour1to2 = let amount = min x (c2 - y)
                     in (x - amount, y + amount)
          -- 6. Pour from bucket 2 to bucket 1
          pour2to1 = let amount = min y (c1 - x)
                     in (x + amount, y - amount)
      in [fill1, fill2, empty1, empty2, pour1to2, pour2to1]
