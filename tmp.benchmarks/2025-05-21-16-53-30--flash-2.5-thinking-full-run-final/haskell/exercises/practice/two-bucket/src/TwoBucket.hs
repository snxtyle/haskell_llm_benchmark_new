module TwoBucket (measure) where

import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import GHC.Real (gcd) -- For gcd
import Data.Foldable (foldl') -- For foldl'

-- State: (current_b1, current_b2, actions)
type State = (Int, Int, Int)
-- Visited state: (current_b1, current_b2)
type VisitedState = (Int, Int)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  -- Target must be positive
  | target <= 0 = Nothing
  -- Target too large for either bucket
  | target > cap1 && target > cap2 = Nothing
  -- Target not reachable if not a multiple of GCD of capacities
  | target `mod` commonDivisor /= 0 = Nothing
  | otherwise = bfs (cap1, cap2) target
  where
    commonDivisor = gcd cap1 cap2

bfs :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
bfs (cap1, cap2) target = go (S.singleton initialState) Set.empty
  where
    -- Initial state: Fill bucket 1 (the starting bucket)
    -- This counts as 1 action.
    initialState :: State
    initialState = (cap1, 0, 1)

    -- The forbidden state (starting bucket empty, other full)
    -- This is (0, cap2) because bucket 1 is assumed to be the starting bucket.
    forbiddenState :: VisitedState
    forbiddenState = (0, cap2)

    go :: S.Seq State -> Set.Set VisitedState -> Maybe (Int, (Int, Int))
    go q visited =
      case S.viewl q of
        S.EmptyL -> Nothing -- No solution found
        (b1, b2, actions) S.:< q' ->
          -- Check if target is reached
          if b1 == target
            then Just (actions, (b1, b2))
          else if b2 == target
            then Just (actions, (b1, b2))
          -- If not target, check if already visited
          else if Set.member (b1, b2) visited
            then go q' visited -- Already visited this state, skip
          else
            let
              visited' = Set.insert (b1, b2) visited
              -- Generate all possible next states from (b1, b2)
              nextStates = generateNextStates (b1, b2) actions cap1 cap2 forbiddenState
              -- Filter out states that have already been visited and add to queue
              q'' = foldl' (S.|>) q' (filter (\(nb1, nb2, _) -> not (Set.member (nb1, nb2) visited')) nextStates)
            in
              go q'' visited'

-- Generates all valid next states from a given state
generateNextStates :: (Int, Int) -> Int -> Int -> Int -> VisitedState -> [State]
generateNextStates (b1, b2) actions cap1 cap2 forbidden
  = catMaybes $
    [
      -- Fill bucket 1
      mkState (cap1, b2),
      -- Fill bucket 2
      mkState (b1, cap2),
      -- Empty bucket 1
      mkState (0, b2),
      -- Empty bucket 2
      mkState (b1, 0),
      -- Pour from bucket 1 to bucket 2
      let
        pourAmount = min b1 (cap2 - b2)
        nb1 = b1 - pourAmount
        nb2 = b2 + pourAmount
      in mkState (nb1, nb2),
      -- Pour from bucket 2 to bucket 1
      let
        pourAmount = min b2 (cap1 - b1)
        nb1 = b1 + pourAmount
        nb2 = b2 - pourAmount
      in mkState (nb1, nb2)
    ]
  where
    -- Helper to create a State, checking for the forbidden state rule
    mkState :: VisitedState -> Maybe State
    mkState (nb1, nb2)
      | (nb1, nb2) == forbidden = Nothing -- Rule: cannot arrive at forbidden state
      | otherwise = Just (nb1, nb2, actions + 1)
