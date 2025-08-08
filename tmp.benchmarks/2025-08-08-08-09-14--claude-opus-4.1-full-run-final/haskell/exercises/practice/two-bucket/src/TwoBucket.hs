module TwoBucket (measure) where

import qualified Data.Set as Set

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (capacity1, capacity2) target = 
    bfs [(capacity1, 0, 1)] (Set.singleton (capacity1, 0))
  where
    -- BFS to find minimum actions
    bfs [] _ = Nothing
    bfs states visited = 
        case filter hasTarget states of
            ((b1, b2, actions):_) -> 
                if b1 == target 
                then Just (actions, (b1, b2))
                else Just (actions, (b2, b1))
            [] -> 
                let nextStates = concatMap (getNextStates visited) states
                    newVisited = foldr (\(b1, b2, _) s -> Set.insert (b1, b2) s) visited nextStates
                in bfs nextStates newVisited
      where
        hasTarget (b1, b2, _) = b1 == target || b2 == target
    
    -- Generate all valid next states from current state
    getNextStates visited (b1, b2, actions) = 
        filter (isValidState visited) $ map (\(nb1, nb2) -> (nb1, nb2, actions + 1))
            [ (capacity1, b2)                                    -- Fill bucket 1
            , (b1, capacity2)                                    -- Fill bucket 2
            , (0, b2)                                            -- Empty bucket 1
            , (b1, 0)                                            -- Empty bucket 2
            , (max 0 (b1 - (capacity2 - b2)), min capacity2 (b2 + b1))  -- Pour 1 into 2
            , (min capacity1 (b1 + b2), max 0 (b2 - (capacity1 - b1)))  -- Pour 2 into 1
            ]
    
    -- Check if state is valid (not visited and doesn't violate constraints)
    isValidState visited (b1, b2, _) = 
        not (Set.member (b1, b2) visited) && 
        not (b1 == 0 && b2 == capacity2)  -- Starting bucket empty and other full is invalid
