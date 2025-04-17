module TwoBucket (measure) where

import qualified Data.Set      as Set
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq ((:<|)))

-- | A state represents the current amount of water that each bucket
--   contains:  (bucket‑1, bucket‑2)
type State = (Int, Int)

-- | Given the capacities of two buckets and a target volume, compute
--   the minimum number of actions required to obtain that volume.
--
--   The first bucket is always the one that is filled first.  The
--   result is
--
--     • Nothing  – the goal cannot be reached.
--     • Just (n, (b, r))
--         n : number of actions
--         b : 1 if bucket‑1 ends with the goal, 2 if bucket‑2 does
--         r : amount of water in the other bucket
--
--   The implementation performs a breadth‑first search over the state
--   space while honouring all restrictions described in the exercise.
measure :: (Int, Int)   -- ^ (capacity of bucket‑1, capacity of bucket‑2)
        -> Int          -- ^ goal (target volume)
        -> Maybe (Int, (Int, Int))
measure (c1, c2) goal
  | goal < 0             = Nothing
  | goal > max c1 c2     = Nothing
  | goal == 0            = Just (0, (2, 0))          -- trivial case
  | goal == c1           = Just (1, (1, 0))          -- first fill already satisfies
  | otherwise            = bfs initialQueue initialVisited
  where
    -- The very first action is to fill bucket‑1.
    startState    :: State
    startState     = (c1, 0)

    initialQueue   = Seq.singleton (startState, 1)   -- (state, actions so far)
    initialVisited = Set.singleton startState

    -- Rule: we must never end an action with bucket‑1 empty AND bucket‑2 full.
    forbidden :: State -> Bool
    forbidden (x, y) = x == 0 && y == c2

    -- Breadth‑first search
    bfs :: Seq (State, Int)      -- ^ queue of states to explore
        -> Set.Set State         -- ^ already visited states
        -> Maybe (Int, (Int, Int))
    bfs Seq.Empty _ = Nothing
    bfs ((s@(x, y), k) :<| qs) visited
      | x == goal = Just (k, (1, y))
      | y == goal = Just (k, (2, x))
      | otherwise =
          let (visited', qs') = foldl (enqueue k) (visited, qs) (nextStates s)
          in bfs qs' visited'

    -- Try to enqueue a successor state if it is new and allowed.
    enqueue :: Int
            -> (Set.Set State, Seq (State, Int))
            -> State
            -> (Set.Set State, Seq (State, Int))
    enqueue movesSoFar (vis, q) st
      | Set.member st vis = (vis, q)        -- seen already
      | forbidden st      = (vis, q)        -- violates rule
      | otherwise         = ( Set.insert st vis
                            , q Seq.|> (st, movesSoFar + 1) )

    -- All legal successor states reachable in a single action.
    nextStates :: State -> [State]
    nextStates (x, y) =
      let fill1       = (c1, y)
          fill2       = (x, c2)
          empty1      = (0,  y)
          empty2      = (x,  0)

          -- Pour bucket‑1 → bucket‑2
          t12         = min x (c2 - y)
          pour12      = (x - t12, y + t12)

          -- Pour bucket‑2 → bucket‑1
          t21         = min y (c1 - x)
          pour21      = (x + t21, y - t21)
      in filter (/= (x, y)) [ fill1, fill2
                            , empty1, empty2
                            , pour12, pour21
                            ]
