module TwoBucket (measure) where

import qualified Data.Set      as Set
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq (..), (|>))

-- | A state is a pair holding the current amounts (in litres) in
--   bucket one and bucket two respectively.
type State = (Int, Int)

-- | Given the capacities of bucket one and bucket two (in litres) and a
-- desired target amount, determine the minimum number of actions that are
-- necessary to arrive at a state in which either bucket contains exactly the
-- target amount.
--
-- The result is
--
--   Just (moves, (bucketNumber, otherBucketLitres))
--
-- where
--
-- * moves              – total number of actions that were carried out
-- * bucketNumber       – 1 if bucket one ends with the target amount, 2 otherwise
-- * otherBucketLitres  – how many litres are left in the bucket that is *not*
--                        holding the target amount
--
-- If it is impossible to reach the target amount the function returns Nothing.
--
-- For the sake of simplicity (see exercise instructions) we always start by
-- filling bucket one first – that first fill already counts as one action.
measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target < 0                 = Nothing
  | target == 0                = Just (0, (1, 0))          -- trivial
  | target > max cap1 cap2     = Nothing                   -- cannot fit
  | target `mod` gcdCap /= 0   = Nothing                   -- Bézout
  | target == cap1             = Just (1, (1, 0))          -- first fill is enough
  | otherwise                  = bfs initialQueue initialVisited
  where
    gcdCap = gcd cap1 cap2

    -- The first mandatory action: fill bucket one
    startState     :: State
    startState     = (cap1, 0)

    initialQueue   :: Seq (State, Int)
    initialQueue   = Seq.singleton (startState, 1)   -- state together with action count

    initialVisited :: Set.Set State
    initialVisited = Set.singleton startState

    -- Breadth‑first search through the state space, each edge has weight 1
    bfs :: Seq (State, Int) -> Set.Set State -> Maybe (Int, (Int, Int))
    bfs Empty _ = Nothing
    bfs (((b1, b2), m) :<| rest) vis
      | b1 == target = Just (m, (1, b2))
      | b2 == target = Just (m, (2, b1))
      | otherwise    =
          let nextStates            = filterAllowed $ neighbours (b1, b2)
              (rest', vis') = foldr
                                (\st (q, s) ->
                                   if Set.member st s
                                     then (q, s)
                                     else (q |> (st, m + 1), Set.insert st s))
                                (rest, vis)
                                nextStates
          in bfs rest' vis'

    ----------------------------------------------------------------------
    -- Generating adjacent (reachable in one action) states

    neighbours :: State -> [State]
    neighbours (b1, b2) =
      [ pourInto2                              -- pour bucket1 -> bucket2
      , pourInto1                              -- pour bucket2 -> bucket1
      , (cap1, b2)                             -- fill bucket one
      , (b1,  cap2)                            -- fill bucket two
      , (0,   b2)                              -- empty bucket one
      , (b1,  0)                               -- empty bucket two
      ]
      where
        pourInto2 = pour b1 cap1 b2 cap2
        pourInto1 = let (newB2, newB1) = pour b2 cap2 b1 cap1
                    in (newB1, newB2)

        -- | Pour from the first bucket into the second bucket.
        --   The result is a pair (newAmountInFrom, newAmountInTo).
        pour :: Int -> Int -> Int -> Int -> (Int, Int)
        pour from fromCap to toCap =
          let space       = toCap - to
              transferred = min from space
          in (from - transferred, to + transferred)

    ----------------------------------------------------------------------
    -- Helper to discard invalid states (rule: cannot end up with starting
    -- bucket empty and the other bucket full) and the start state itself
    -- which would otherwise re‑enter the queue.
    filterAllowed :: [State] -> [State]
    filterAllowed =
      filter (\st@(x, y) -> st /= startState        -- avoid self‑loop
                         && not (x == 0 && y == cap2))
