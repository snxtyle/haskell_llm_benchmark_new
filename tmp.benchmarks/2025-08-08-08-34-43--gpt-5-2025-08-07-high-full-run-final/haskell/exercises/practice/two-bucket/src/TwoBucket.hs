module TwoBucket (measure) where

import Data.List (nub)

-- | Given capacities of two buckets (bucket one, bucket two) and a target,
--   compute the minimal number of actions starting with filling bucket one,
--   along with which bucket ends with the target (1 or 2) and how many liters
--   are left in the other bucket.
--
--   Returns:
--     Just (actions, (goalBucketNumber, otherBucketLiters))
--   or
--     Nothing if the target cannot be reached under the rules.
measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target <= 0 = Nothing
  | target > max cap1 cap2 = Nothing
  | otherwise = bfs 1 [(cap1, 0)] [(cap1, 0)]
  where
    -- BFS by levels (each level = one action) to ensure minimal actions.
    -- At each level, prefer solutions where bucket two has the target before
    -- solutions where bucket one has the target.
    bfs :: Int -> [(Int, Int)] -> [(Int, Int)] -> Maybe (Int, (Int, Int))
    bfs _ [] _ = Nothing
    bfs steps frontier visited =
      case pickSolution frontier of
        Just res -> Just (steps, res)
        Nothing  ->
          let nextStates =
                nub
                  [ s
                  | st <- frontier
                  , s <- neighbors st
                  , s /= (0, cap2)            -- disallow ending at (empty start, other full)
                  , s `notElem` visited
                  ]
              newVisited = nextStates ++ visited
          in bfs (steps + 1) nextStates newVisited

    -- Prefer bucket two having the target; if none, then bucket one.
    pickSolution :: [(Int, Int)] -> Maybe (Int, Int)
    pickSolution frontier =
      case [ (2, x) | (x, y) <- frontier, y == target ] of
        (res:_) -> Just res
        [] ->
          case [ (1, y) | (x, y) <- frontier, x == target ] of
            (res:_) -> Just res
            []      -> Nothing

    neighbors :: (Int, Int) -> [(Int, Int)]
    neighbors (x, y) =
      let -- Fill bucket one (if not already full)
          fill1   = [ (cap1, y)        | x < cap1 ]
          -- Fill bucket two (if not already full)
          fill2   = [ (x, cap2)        | y < cap2 ]
          -- Empty bucket one (if not already empty)
          empty1  = [ (0, y)           | x > 0 ]
          -- Empty bucket two (if not already empty)
          empty2  = [ (x, 0)           | y > 0 ]
          -- Pour bucket one -> bucket two
          pour12  =
            let delta = min x (cap2 - y)
            in [ (x - delta, y + delta) | delta > 0 ]
          -- Pour bucket two -> bucket one
          pour21  =
            let delta = min y (cap1 - x)
            in [ (x + delta, y - delta) | delta > 0 ]
      in fill1 ++ fill2 ++ empty1 ++ empty2 ++ pour12 ++ pour21
