module TwoBucket (measure) where

import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

type State = (Int, Int)
type Capacity = (Int, Int)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target > max cap1 cap2 = Nothing
  | target == 0 = Just (1, (0, 0))
  | otherwise = search [(1, (cap1, 0))] (Set.singleton (cap1, 0))
  where
    search :: [(Int, State)] -> Set.Set State -> Maybe (Int, (Int, Int))
    search [] _ = Nothing
    search ((steps, state@(b1, b2)):queue) visited
      | b1 == target = Just (steps, state)
      | b2 == target = Just (steps, state)
      | otherwise = 
          let nextStates = generateNextStates (cap1, cap2) state
              validStates = filter (\s -> s `Set.notMember` visited && not (isForbidden s)) nextStates
              newVisited = foldr Set.insert visited validStates
              newQueue = queue ++ [(steps + 1, s) | s <- validStates]
          in search newQueue newVisited
    
    -- The forbidden state is when the starting bucket (bucket 1) is empty and the other is full
    isForbidden :: State -> Bool
    isForbidden (0, b2) = b2 == cap2
    isForbidden _ = False

generateNextStates :: Capacity -> State -> [State]
generateNextStates (cap1, cap2) (b1, b2) =
  [ -- Fill bucket 1
    (cap1, b2)
  , -- Fill bucket 2
    (b1, cap2)
  , -- Empty bucket 1
    (0, b2)
  , -- Empty bucket 2
    (b1, 0)
  , -- Pour bucket 1 into bucket 2
    let amount = min b1 (cap2 - b2)
    in (b1 - amount, b2 + amount)
  , -- Pour bucket 2 into bucket 1
    let amount = min b2 (cap1 - b1)
    in (b1 + amount, b2 - amount)
  ]
