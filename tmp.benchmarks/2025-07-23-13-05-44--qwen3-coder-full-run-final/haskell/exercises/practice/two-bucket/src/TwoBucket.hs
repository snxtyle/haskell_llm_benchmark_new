module TwoBucket (measure) where

import Data.List (find)
import qualified Data.Set as Set

data Bucket = Bucket1 | Bucket2 deriving (Show, Eq)
data State = State { bucket1 :: Int, bucket2 :: Int } deriving (Show, Eq, Ord)

measure :: (Int, Int) -> Int -> Bucket -> Maybe (Int, Bucket, Int)
measure (capacity1, capacity2) target startBucket = 
    case bfs (Set.singleton startState) [(startState, 1)] of
        Nothing -> Nothing
        Just (state, actions) -> 
            if bucket1 state == target
            then Just (actions, Bucket1, bucket2 state)
            else Just (actions, Bucket2, bucket1 state)
  where
    startState = case startBucket of
        Bucket1 -> State capacity1 0
        Bucket2 -> State 0 capacity2
    
    -- BFS to find the minimum number of actions
    bfs :: Set.Set State -> [(State, Int)] -> Maybe (State, Int)
    bfs visited [] = Nothing
    bfs visited ((state, actions):queue)
        | bucket1 state == target || bucket2 state == target = Just (state, actions)
        | otherwise = 
            let nextStates = getNextStates (capacity1, capacity2) state
                validNextStates = filter (isValidState (capacity1, capacity2) startState) nextStates
                unvisitedStates = filter (\s -> Set.notMember s visited) validNextStates
                newVisited = Set.union visited (Set.fromList unvisitedStates)
                newQueue = queue ++ [(s, actions + 1) | s <- unvisitedStates]
            in bfs newVisited newQueue

-- Generate all possible next states from the current state
getNextStates :: (Int, Int) -> State -> [State]
getNextStates (cap1, cap2) (State b1 b2) =
    [ State cap1 b2  -- Fill bucket 1
    , State b1 cap2  -- Fill bucket 2
    , State 0 b2     -- Empty bucket 1
    , State b1 0     -- Empty bucket 2
    , State (max 0 (b1 - (cap2 - b2))) (min cap2 (b2 + b1))  -- Pour bucket 1 to bucket 2
    , State (min cap1 (b1 + b2)) (max 0 (b2 - (cap1 - b1)))  -- Pour bucket 2 to bucket 1
    ]

-- Check if a state is valid according to the rules
isValidState :: (Int, Int) -> State -> State -> Bool
isValidState (cap1, cap2) startState currentState = 
    currentState /= State 0 cap2 || startState /= State cap1 0
