module TwoBucket (measure) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl')

type State = (Int, Int)

-- | Given capacities (cap1, cap2) and a target amount,
--   starting by filling bucket one first,
--   compute:
--   - the total number of actions to reach the target (including the first fill)
--   - which bucket ends with the target (1 or 2)
--   - how many liters remain in the other bucket
--
--   Returns Nothing if it is impossible under the rules.
measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target < 0 = Nothing
  | cap1 <= 0 || cap2 <= 0 = Nothing
  | target > max cap1 cap2 = Nothing
  | otherwise = bfs initialQueue initialVisited
  where
    -- Invalid state as per rule: after any action, may not be (start empty, other full)
    invalid :: State -> Bool
    invalid (a, b) = a == 0 && b == cap2

    goal :: State -> Bool
    goal (a, b) = a == target || b == target

    -- Generate all possible single-action neighbors from a state
    neighbors :: State -> [State]
    neighbors (a, b) =
      filter distinctAndValid
        [ fill1, fill2
        , empty1, empty2
        , pour12, pour21
        ]
      where
        distinctAndValid s' = s' /= (a, b) && not (invalid s')
        fill1 = (cap1, b)
        fill2 = (a, cap2)
        empty1 = (0, b)
        empty2 = (a, 0)
        pour12 =
          let t = min a (cap2 - b)
          in (a - t, b + t)
        pour21 =
          let t = min b (cap1 - a)
          in (a + t, b - t)

    -- Start by filling bucket one (counts as the first action)
    initial :: State
    initial = (cap1, 0)

    initialMoves :: Int
    initialMoves = 1

    initialQueue :: [(State, Int)]
    initialQueue = [(initial, initialMoves)]

    initialVisited :: Set State
    initialVisited = Set.singleton initial

    bfs :: [(State, Int)] -> Set State -> Maybe (Int, (Int, Int))
    bfs [] _ = Nothing
    bfs ((s@(a, b), moves) : qs) vis
      | goal s =
          if a == target
            then Just (moves, (1, b))
            else Just (moves, (2, a))
      | otherwise =
          let nextStates = filter (`Set.notMember` vis) (neighbors s)
              vis' = foldl' (flip Set.insert) vis nextStates
              qs' = qs ++ [ (ns, moves + 1) | ns <- nextStates ]
          in bfs qs' vis'
