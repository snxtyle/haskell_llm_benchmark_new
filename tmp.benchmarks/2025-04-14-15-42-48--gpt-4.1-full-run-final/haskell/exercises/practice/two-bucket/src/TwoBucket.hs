module TwoBucket (measure) where

import Data.List (nub)

-- | measure (bucket1Capacity, bucket2Capacity) target startBucket
-- Returns: Just (actions, (goalBucket, otherBucketLiters))
--   where goalBucket is 1 or 2 (which bucket ends up with the target)
--   and otherBucketLiters is the amount in the other bucket
-- Returns Nothing if impossible
measure :: (Int, Int) -> Int -> Int -> Maybe (Int, (Int, Int))
measure (c1, c2) target startBucket
  | target > c1 && target > c2 = Nothing
  | target == c1 && startBucket == 1 = Just (1, (1, 0))
  | target == c2 && startBucket == 2 = Just (1, (2, 0))
  | otherwise =
      let (sc1, sc2, s1, s2) = if startBucket == 1
                               then (c1, c2, (c1, 0), 1)
                               else (c2, c1, (0, c2), 2)
          res = bfs sc1 sc2 target s1 s2 1 startBucket
      in fmap (\(a, x, y) -> if x == target then (a, (startBucket, y)) else (a, (otherBucket startBucket, x))) res
  where
    otherBucket 1 = 2
    otherBucket 2 = 1
    otherBucket _ = error "Invalid bucket"

type State = (Int, Int) -- (bucket1, bucket2)

-- BFS to find minimal steps to reach target in either bucket
bfs :: Int -> Int -> Int -> State -> Int -> Int -> Int -> Maybe (Int, Int, Int)
bfs c1 c2 target (b1, b2) actions startBucket = bfs' [((b1, b2), actions)] []
  where
    bfs' [] _ = Nothing
    bfs' (((x, y), n):queue) visited
      | x == target = Just (n, x, y)
      | y == target = Just (n, x, y)
      | (x, y) `elem` visited = bfs' queue visited
      | otherwise =
          let nexts = nextStates c1 c2 (x, y) startBucket
              queue' = queue ++ [ (ns, n+1) | ns <- nexts ]
          in bfs' queue' ((x, y):visited)

-- Generate all valid next states from current state, respecting the rules
nextStates :: Int -> Int -> State -> Int -> [State]
nextStates c1 c2 (b1, b2) startBucket =
  nub $ filter validState
    [ (c1, b2)   -- fill bucket 1
    , (b1, c2)   -- fill bucket 2
    , (0, b2)    -- empty bucket 1
    , (b1, 0)    -- empty bucket 2
    , pour1to2 b1 b2
    , pour2to1 b1 b2
    ]
  where
    -- Pour bucket 1 into bucket 2
    pour1to2 b1 b2 =
      let pour = min b1 (c2 - b2)
      in (b1 - pour, b2 + pour)
    -- Pour bucket 2 into bucket 1
    pour2to1 b1 b2 =
      let pour = min b2 (c1 - b1)
      in (b1 + pour, b2 - pour)
    -- Rule: may not arrive at a state where the starting bucket is empty and the other is full
    validState (x, y)
      | startBucket == 1 = not (x == 0 && y == c2)
      | startBucket == 2 = not (y == 0 && x == c1)
      | otherwise = True
