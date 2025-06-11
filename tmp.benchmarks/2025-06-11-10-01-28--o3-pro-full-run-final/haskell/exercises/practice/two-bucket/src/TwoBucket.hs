module TwoBucket (measure) where

import           Data.Sequence  (Seq (..), (|>))
import qualified Data.Sequence  as Seq
import           Data.Set       (Set)
import qualified Data.Set       as Set

-- | Given the capacities of two buckets (bucket one, bucket two) and a target
--   amount, determine how many actions are required to reach the target,
--   which bucket ends with the target amount (1 or 2) and how much is left in
--   the other bucket.
--
--   The returned tuple has the following meaning:
--
--   * Int  : total number of actions that have been performed
--   * (Int,Int) :
--       - first component  : 1 if bucket one contains the target amount,
--                            2 if bucket two contains the target amount
--       - second component : amount of water contained in the other bucket
--
--   If it is impossible to reach the target, the result is 'Nothing'.
--
--   The algorithm follows the rules specified in the exercise – in particular
--   we never allow the state (0, full‐capacity-of-bucket-two) to occur,
--   because bucket one is the designated “starting bucket”.
measure :: (Int, Int)        -- ^ capacities of bucket one and bucket two
        -> Int               -- ^ desired target amount
        -> Maybe (Int, (Int, Int))
measure (capA, capB) target
  | target < 0 || target > max capA capB = Nothing
  | target == capA = Just (1, (1, 0))        -- first fill already satisfies
  | otherwise      = bfs initialQueue initialVisited
  where
    -- the very first mandated action: fill bucket one
    initialState   = (capA, 0)
    initialMoves   = 1
    initialQueue   = Seq.singleton (initialState, initialMoves)
    initialVisited = Set.fromList [(0, 0), initialState]

    bfs :: Seq ((Int, Int), Int) -> Set (Int, Int) -> Maybe (Int, (Int, Int))
    bfs Empty _ = Nothing
    bfs ((state@(a, b), moves) :<| rest) visited
      | a == target = Just (moves, (1, b))
      | b == target = Just (moves, (2, a))
      | otherwise   = bfs newQueue newVisited
      where
        nextStates   = successors state
        unseenStates = filter (`Set.notMember` visited) nextStates
        newVisited   = foldr Set.insert visited unseenStates
        newQueue     = foldl (\q s -> q |> (s, moves + 1)) rest unseenStates

    -- Generate all legal successor states reachable in a single action
    successors :: (Int, Int) -> [(Int, Int)]
    successors (a, b) =
      filter (/= (a, b))              -- exclude the current state itself
      . filter validState             -- enforce exercise specific rule
      $ concat
          [ [ pourAToB a b ]
          , [ pourBToA a b ]
          , [ (capA, b)      | a < capA ]        -- fill bucket one
          , [ (a, capB)      | b < capB, a /= 0 ]-- fill bucket two
          , [ (0, b)         | a > 0 ]           -- empty bucket one
          , [ (a, 0)         | b > 0 ]           -- empty bucket two
          ]
      where
        validState (0, fullB) = fullB /= capB    -- disallow (0, capB)
        validState _          = True

        pourAToB :: Int -> Int -> (Int, Int)
        pourAToB x y =
          let space   = capB - y
              transfer = min x space
          in (x - transfer, y + transfer)

        pourBToA :: Int -> Int -> (Int, Int)
        pourBToA x y =
          let space   = capA - x
              transfer = min y space
          in (x + transfer, y - transfer)
