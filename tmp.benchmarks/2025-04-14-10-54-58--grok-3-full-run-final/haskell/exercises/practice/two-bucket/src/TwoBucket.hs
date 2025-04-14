module TwoBucket (measure) where

import Data.Set (Set)
import qualified Data.Set as Set

-- State represents the current amount in each bucket and the number of actions taken
type State = ((Int, Int), Int)

-- measure returns the number of actions, which bucket contains the target, and the amount in the other bucket
measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target = 
    if target > max cap1 cap2 || target < 0
    then Nothing
    else bfs [((0, 0), 0)] (Set.singleton (0, 0))
  where
    bfs :: [State] -> Set (Int, Int) -> Maybe (Int, (Int, Int))
    bfs [] _ = Nothing
    bfs (((b1, b2), actions):restQueue) visited =
        if b1 == target then Just (actions, (1, b2))
        else if b2 == target then Just (actions, (2, b1))
        else let newStates = generateStates (cap1, cap2) (b1, b2) actions visited
                 newVisited = foldr (uncurry Set.insert) visited (map fst newStates)
                 newQueue = restQueue ++ newStates
             in bfs newQueue newVisited

    -- Generate all possible next states from current state
    generateStates :: (Int, Int) -> (Int, Int) -> Int -> Set (Int, Int) -> [State]
    generateStates (cap1, cap2) (b1, b2) actions visited = 
        let newActions = actions + 1
            possibleStates = filter (not . flip Set.member visited . fst) $
                filter (not . isInvalidStartState) [
                    -- Fill bucket 1
                    ((cap1, b2), newActions),
                    -- Fill bucket 2
                    ((b1, cap2), newActions),
                    -- Empty bucket 1
                    ((0, b2), newActions),
                    -- Empty bucket 2
                    ((b1, 0), newActions),
                    -- Pour from 1 to 2
                    let spaceIn2 = cap2 - b2
                        pourAmount = min b1 spaceIn2
                    in ((b1 - pourAmount, b2 + pourAmount), newActions),
                    -- Pour from 2 to 1
                    let spaceIn1 = cap1 - b1
                        pourAmount = min b2 spaceIn1
                    in ((b1 + pourAmount, b2 - pourAmount), newActions)
                ]
        in possibleStates

    -- Check if state violates the rule about starting bucket empty and other full
    -- Assuming bucket 1 is the starting bucket as per note in instructions
    isInvalidStartState :: State -> Bool
    isInvalidStartState ((b1, b2), _) = b1 == 0 && b2 == cap2
