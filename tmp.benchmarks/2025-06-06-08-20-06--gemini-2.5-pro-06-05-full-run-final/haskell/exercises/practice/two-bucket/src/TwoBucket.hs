module TwoBucket (measure) where

import qualified Data.Set as Set

-- Using a list as a queue for BFS. Appending to a list is O(n), so this is not
-- the most performant queue, but it is simple and sufficient for the constraints
-- of this problem, and avoids extra dependencies like `Data.Sequence`.

type BucketState = (Int, Int) -- (level of bucket one, level of bucket two)
type State = (Int, BucketState) -- (number of moves, current bucket state)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target =
    -- The problem is specified such that the caller will swap the bucket capacities
    -- to indicate which bucket to start with. This implementation assumes we always
    -- start by filling the first bucket (with capacity cap1).
    let
        -- The first move is to fill bucket one.
        startState = (1, (cap1, 0))
        -- The forbidden state when starting with bucket one is (0, cap2).
        forbidden = (0, cap2)
        -- Initial queue for BFS.
        q = [startState]
        -- Set of visited bucket level combinations.
        visited = Set.singleton (cap1, 0)
    in bfs (cap1, cap2) target forbidden q visited

bfs :: (Int, Int) -> Int -> BucketState -> [State] -> Set.Set BucketState -> Maybe (Int, BucketState)
bfs _ _ _ [] _ = Nothing -- Queue is empty, no solution found.
bfs caps@(cap1, cap2) target forbidden (current:queue) visited =
    let (moves, levels@(l1, l2)) = current
    in if l1 == target || l2 == target
       then Just (moves, levels)
       else
           let
               -- All possible next states from (l1, l2) based on the allowed actions.
               nextBucketStates =
                   [ -- Fill bucket 1
                     (cap1, l2)
                   , -- Fill bucket 2
                     (l1, cap2)
                   , -- Empty bucket 1
                     (0, l2)
                   , -- Empty bucket 2
                     (l1, 0)
                   , -- Pour from bucket 1 to bucket 2
                     let pourAmount = min l1 (cap2 - l2)
                     in (l1 - pourAmount, l2 + pourAmount)
                   , -- Pour from bucket 2 to bucket 1
                     let pourAmount = min l2 (cap1 - l1)
                     in (l1 + pourAmount, l2 - pourAmount)
                   ]

               -- Filter out states that are the same as current, forbidden, or already visited.
               newValidStates = filter (\lvl -> lvl /= levels && lvl /= forbidden && not (Set.member lvl visited)) nextBucketStates

               -- Create new states for the queue, incrementing the move count.
               newQueueStates = map (\lvl -> (moves + 1, lvl)) newValidStates

               -- Add the new states to the queue for the next iteration of the search.
               newQueue = queue ++ newQueueStates
               -- Add the new bucket levels to the visited set to avoid cycles.
               newVisited = foldr Set.insert visited (map snd newQueueStates)

           in bfs caps target forbidden newQueue newVisited
