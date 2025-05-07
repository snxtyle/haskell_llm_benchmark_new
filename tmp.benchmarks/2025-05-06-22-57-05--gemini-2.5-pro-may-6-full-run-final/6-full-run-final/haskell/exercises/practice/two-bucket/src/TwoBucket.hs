module TwoBucket (measure) where

import qualified Data.Set as Set
import qualified Data.Sequence as Seq

-- BucketState stores the amount of water in bucket one and bucket two.
type BucketState = (Int, Int) -- (litersInB1, litersInB2)

-- BFSQueueItem stores a state and the number of actions taken to reach it.
type BFSQueueItem = (BucketState, Int) -- ((b1, b2), actions)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (capacityB1, capacityB2) targetLiters =
    -- As per problem note: bucket one (capacityB1) is the starting bucket.
    -- The first action is to fill this bucket.
    let initialActions = 1
        initialBucketState = (capacityB1, 0)
        
        -- The state (0, capacityB2) is forbidden if bucket one is the starting bucket.
        forbiddenState = (0, capacityB2)
    in
    -- Start BFS. The queue initially contains the state after the first fill.
    -- The visited set also contains this initial state.
    -- No special pre-checks for targetLiters == 0 or initial state matching target,
    -- as the BFS will correctly handle these cases when it processes the initial state.
    bfs (capacityB1, capacityB2) targetLiters forbiddenState
        (Seq.singleton (initialBucketState, initialActions))
        (Set.singleton initialBucketState)

bfs :: (Int, Int)             -- (capacityB1, capacityB2)
    -> Int                    -- targetLiters
    -> BucketState            -- forbiddenState, e.g., (0, capacityB2) for this setup
    -> Seq.Seq BFSQueueItem   -- BFS queue
    -> Set.Set BucketState    -- Visited states
    -> Maybe (Int, (Int, Int)) -- Result: (totalActions, (goalBucketID, otherBucketLiters))
bfs (cap1, cap2) target forbiddenSt queue visited =
    case Seq.viewl queue of
        Seq.EmptyL -> Nothing -- Queue is empty, target not found

        -- Dequeue the current state and actions
        ((currentB1, currentB2), actions) Seq.:< restOfQueue ->

            -- Check if target is reached in the current state.
            if currentB1 == target then
                Just (actions, (1, currentB2)) -- Target in bucket one
            else if currentB2 == target then
                Just (actions, (2, currentB1)) -- Target in bucket two
            
            -- If target not reached, generate next possible states.
            else
                let
                    nextActions = actions + 1

                    -- All 6 possible actions:
                    -- 1. Fill bucket one
                    s_fillB1 = (cap1, currentB2)
                    -- 2. Fill bucket two
                    s_fillB2 = (currentB1, cap2)
                    -- 3. Empty bucket one
                    s_emptyB1 = (0, currentB2)
                    -- 4. Empty bucket two
                    s_emptyB2 = (currentB1, 0)
                    -- 5. Pour bucket one into bucket two
                    pourB1toB2Amount = min currentB1 (cap2 - currentB2)
                    s_pourB1toB2 = (currentB1 - pourB1toB2Amount, currentB2 + pourB1toB2Amount)
                    -- 6. Pour bucket two into bucket one
                    pourB2toB1Amount = min currentB2 (cap1 - currentB1)
                    s_pourB2toB1 = (currentB1 + pourB2toB1Amount, currentB2 - pourB2toB1Amount)

                    potentialNextStates = [s_fillB1, s_fillB2, s_emptyB1, s_emptyB2, s_pourB1toB2, s_pourB2toB1]
                    
                    -- Filter out states that are forbidden or already visited.
                    newStatesToExplore = filter (\s -> s /= forbiddenSt && not (Set.member s visited)) potentialNextStates
                    
                    -- Update queue and visited set for the next BFS step.
                    updatedQueue = foldl (\q s -> q Seq.|> (s, nextActions)) restOfQueue newStatesToExplore
                    updatedVisited = foldl (\vSet s -> Set.insert s vSet) visited newStatesToExplore
                in
                bfs (cap1, cap2) target forbiddenSt updatedQueue updatedVisited
