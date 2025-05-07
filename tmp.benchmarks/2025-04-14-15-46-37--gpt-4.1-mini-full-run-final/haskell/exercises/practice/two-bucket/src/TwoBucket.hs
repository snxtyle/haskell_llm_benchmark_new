module TwoBucket (measure) where

import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

-- State: (bucket1, bucket2)
-- Action: one step changing the state
-- We want to find the minimal number of actions to reach target liters in either bucket,
-- starting with bucket1 filled first.

-- The problem states we can assume starting bucket is bucket1.
-- Actions:
-- 1) Pour from one bucket to the other until source empty or dest full
-- 2) Empty a bucket
-- 3) Fill a bucket

-- After any action, the state must not be (start bucket empty, other bucket full)

type Capacity = (Int, Int)
type State = (Int, Int)
type Step = (Int, State) -- number of actions, state

measure :: Capacity -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target > max cap1 cap2 = Nothing
  | target `mod` gcd cap1 cap2 /= 0 = Nothing
  | otherwise = bfs Set.empty [(1, (cap1, 0))] -- start with bucket1 filled (1 action)
  where
    startBucket = 1 -- bucket1 is start bucket

    -- Check if state is invalid: start bucket empty and other bucket full
    invalidState :: State -> Bool
    invalidState (b1, b2) = (b1 == 0 && b2 == cap2)

    -- Check if target reached in either bucket
    isTarget :: State -> Bool
    isTarget (b1, b2) = b1 == target || b2 == target

    -- Generate all possible next states from current state
    nextStates :: State -> [State]
    nextStates (b1, b2) = filter (not . invalidState) $
      -- Actions:
      -- 1) Pour bucket1 -> bucket2
      let pour12 = let pourAmount = min b1 (cap2 - b2)
                   in (b1 - pourAmount, b2 + pourAmount)
          -- Pour bucket2 -> bucket1
          pour21 = let pourAmount = min b2 (cap1 - b1)
                   in (b1 + pourAmount, b2 - pourAmount)
          -- 2) Empty bucket1
          empty1 = (0, b2)
          -- Empty bucket2
          empty2 = (b1, 0)
          -- 3) Fill bucket1
          fill1 = (cap1, b2)
          -- Fill bucket2
          fill2 = (b1, cap2)
      in [pour12, pour21, empty1, empty2, fill1, fill2]

    bfs :: Set.Set State -> [Step] -> Maybe (Int, (Int, Int))
    bfs _ [] = Nothing
    bfs visited ((steps, state@(b1,b2)):queue)
      | isTarget state = Just (steps, if b1 == target then (1, b1, b2) else (2, b1, b2)) >>= \(s, (bucket, x, y)) -> Just (s, if bucket == 1 then (x,y) else (x,y))
      | state `Set.member` visited = bfs visited queue
      | otherwise =
          let visited' = Set.insert state visited
              next = [(steps + 1, st) | st <- nextStates state, st `Set.notMember` visited']
          in bfs visited' (queue ++ next)
