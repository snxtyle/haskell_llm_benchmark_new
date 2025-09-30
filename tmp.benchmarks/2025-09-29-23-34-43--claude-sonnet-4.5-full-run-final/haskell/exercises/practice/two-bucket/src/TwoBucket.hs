module TwoBucket (measure) where

import Data.List (find)
import qualified Data.Set as Set

-- State: (bucket1, bucket2, moves)
type State = (Int, Int, Int)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target > max cap1 cap2 = Nothing
  | target == cap1 = Just (1, (cap1, 0))
  | target == cap2 = Just (1, (0, cap2))
  | otherwise = bfs [(cap1, 0, 1)] (Set.singleton (cap1, 0))
  where
    bfs :: [State] -> Set.Set (Int, Int) -> Maybe (Int, (Int, Int))
    bfs [] _ = Nothing
    bfs ((b1, b2, moves):rest) visited
      | b1 == target = Just (moves, (b1, b2))
      | b2 == target = Just (moves, (b1, b2))
      | otherwise = 
          let nextStates = generateNextStates (b1, b2, moves)
              validStates = filter (isValid visited) nextStates
              newVisited = foldr (\(x, y, _) acc -> Set.insert (x, y) acc) visited validStates
          in bfs (rest ++ validStates) newVisited

    generateNextStates :: State -> [State]
    generateNextStates (b1, b2, moves) =
      [ -- Fill bucket 1
        (cap1, b2, moves + 1)
        -- Fill bucket 2
      , (b1, cap2, moves + 1)
        -- Empty bucket 1
      , (0, b2, moves + 1)
        -- Empty bucket 2
      , (b1, 0, moves + 1)
        -- Pour from bucket 1 to bucket 2
      , let amount = min b1 (cap2 - b2)
        in (b1 - amount, b2 + amount, moves + 1)
        -- Pour from bucket 2 to bucket 1
      , let amount = min b2 (cap1 - b1)
        in (b1 + amount, b2 - amount, moves + 1)
      ]

    isValid :: Set.Set (Int, Int) -> State -> Bool
    isValid visited (b1, b2, _) =
      not (Set.member (b1, b2) visited) &&  -- Not visited
      not (b1 == 0 && b2 == cap2)           -- Not (empty starting bucket, full other bucket)
