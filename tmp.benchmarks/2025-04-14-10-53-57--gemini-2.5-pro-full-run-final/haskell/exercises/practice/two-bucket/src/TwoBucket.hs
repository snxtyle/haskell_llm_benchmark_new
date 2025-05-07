{-# LANGUAGE MultiWayIf #-}

module TwoBucket (measure) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Set as Set
import Data.Set (Set)

-- | Represents the state of the two buckets.
type BucketState = (Int, Int) -- (liters_in_bucket1, liters_in_bucket2)

-- | Represents an item in the BFS queue: (number_of_moves, current_state).
type QueueItem = (Int, BucketState)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
    -- Pre-checks for impossible scenarios
    | target <= 0                                                       = Nothing -- Target must be positive
    | target > max cap1 cap2                                            = Nothing -- Target larger than the biggest bucket
    | gcd cap1 cap2 /= 0 && target `mod` gcd cap1 cap2 /= 0             = Nothing -- Target not divisible by GCD of capacities
    -- Optimization: If target is capacity1, the first move solves it.
    | target == cap1                                                    = Just (1, (1, 0))
    -- Optimization: If target is capacity2 and cap1 can fill it directly? No, let BFS handle.
    -- Start BFS, assuming bucket 1 is filled first (move 1)
    | otherwise = bfs (Seq.singleton (1, (cap1, 0))) (Set.singleton (cap1, 0))
  where
    -- The forbidden state when starting with bucket 1
    forbiddenState :: BucketState
    forbiddenState = (0, cap2)

    -- Breadth-First Search implementation
    bfs :: Seq QueueItem -> Set BucketState -> Maybe (Int, (Int, Int))
    bfs queue visited =
      case Seq.viewl queue of
        -- Queue is empty, target not found
        EmptyL -> Nothing
        -- Dequeue the next state to explore
        (moves, state@(b1, b2)) :< rest ->
          -- Check if the target is reached
          if | b1 == target -> Just (moves, (1, b2)) -- Target in bucket 1
             | b2 == target -> Just (moves, (2, b1)) -- Target in bucket 2
             | otherwise ->
                 -- Generate all possible next states from the current state
                 let nextPossibleStates = generateNextStates state
                     -- Filter out invalid, forbidden, or already visited states
                     validNewStates = filter (\s -> s /= forbiddenState && not (s `Set.member` visited)) nextPossibleStates
                     -- Add the new valid states to the visited set
                     newVisited = foldr Set.insert visited validNewStates
                     -- Enqueue the new valid states with incremented move count
                     newQueue = foldl (\q s -> q |> (moves + 1, s)) rest validNewStates
                 -- Continue BFS with the updated queue and visited set
                 in bfs newQueue newVisited

    -- Generates all 6 possible next states from a given state
    generateNextStates :: BucketState -> [BucketState]
    generateNextStates (b1, b2) =
      [ -- 1. Fill bucket 1 completely
        (cap1, b2)
        -- 2. Fill bucket 2 completely
      , (b1, cap2)
        -- 3. Empty bucket 1
      , (0, b2)
        -- 4. Empty bucket 2
      , (b1, 0)
        -- 5. Pour bucket 1 into bucket 2 until 1 is empty or 2 is full
      , let pourAmount12 = min b1 (cap2 - b2)
        in (b1 - pourAmount12, b2 + pourAmount12)
        -- 6. Pour bucket 2 into bucket 1 until 2 is empty or 1 is full
      , let pourAmount21 = min b2 (cap1 - b1)
        in (b1 + pourAmount21, b2 - pourAmount21)
      ]
