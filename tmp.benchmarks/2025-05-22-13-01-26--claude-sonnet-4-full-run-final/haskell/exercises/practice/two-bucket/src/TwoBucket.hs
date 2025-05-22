module TwoBucket (measure) where

import qualified Data.Set as Set

-- State: (bucket1, bucket2, actions)
type State = (Int, Int, Int)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target > max cap1 cap2 = Nothing
  | target == 0 = Just (0, (0, 0))
  | otherwise = 
      let result1 = bfsStartWith1 (cap1, cap2) target
          result2 = bfsStartWith2 (cap1, cap2) target
      in case (result1, result2) of
           (Just r1, Just r2) -> Just (if fst r1 <= fst r2 then r1 else r2)
           (Just r1, Nothing) -> Just r1
           (Nothing, Just r2) -> Just r2
           (Nothing, Nothing) -> Nothing

-- BFS starting by filling bucket 1 first
bfsStartWith1 :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
bfsStartWith1 (cap1, cap2) target = search [(cap1, 0, 1)] Set.empty
  where
    search [] _ = Nothing
    search ((b1, b2, actions):queue) visited
      | b1 == target = Just (actions, (b1, b2))
      | b2 == target = Just (actions, (b1, b2))
      | Set.member (b1, b2) visited = search queue visited
      | otherwise = 
          let newVisited = Set.insert (b1, b2) visited
              nextStates = generateNextStates (b1, b2) actions cap1 cap2
              validStates = filter (isValidState1 cap1 cap2) nextStates
              newQueue = queue ++ validStates
          in search newQueue newVisited

-- BFS starting by filling bucket 2 first
bfsStartWith2 :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
bfsStartWith2 (cap1, cap2) target = search [(0, cap2, 1)] Set.empty
  where
    search [] _ = Nothing
    search ((b1, b2, actions):queue) visited
      | b1 == target = Just (actions, (b1, b2))
      | b2 == target = Just (actions, (b1, b2))
      | Set.member (b1, b2) visited = search queue visited
      | otherwise = 
          let newVisited = Set.insert (b1, b2) visited
              nextStates = generateNextStates (b1, b2) actions cap1 cap2
              validStates = filter (isValidState2 cap1 cap2) nextStates
              newQueue = queue ++ validStates
          in search newQueue newVisited

generateNextStates :: (Int, Int) -> Int -> Int -> Int -> [State]
generateNextStates (b1, b2) actions cap1 cap2 =
  [ (cap1, b2, actions + 1)  -- Fill bucket 1
  , (b1, cap2, actions + 1)  -- Fill bucket 2
  , (0, b2, actions + 1)     -- Empty bucket 1
  , (b1, 0, actions + 1)     -- Empty bucket 2
  , pourB1ToB2               -- Pour bucket 1 into bucket 2
  , pourB2ToB1               -- Pour bucket 2 into bucket 1
  ]
  where
    pourB1ToB2 = 
      let amount = min b1 (cap2 - b2)
      in (b1 - amount, b2 + amount, actions + 1)
    
    pourB2ToB1 = 
      let amount = min b2 (cap1 - b1)
      in (b1 + amount, b2 - amount, actions + 1)

-- Validation for starting with bucket 1: cannot have bucket 1 empty and bucket 2 full
isValidState1 :: Int -> Int -> State -> Bool
isValidState1 cap1 cap2 (b1, b2, _) =
  b1 >= 0 && b1 <= cap1 && b2 >= 0 && b2 <= cap2 &&
  not (b1 == 0 && b2 == cap2)

-- Validation for starting with bucket 2: cannot have bucket 2 empty and bucket 1 full
isValidState2 :: Int -> Int -> State -> Bool
isValidState2 cap1 cap2 (b1, b2, _) =
  b1 >= 0 && b1 <= cap1 && b2 >= 0 && b2 <= cap2 &&
  not (b2 == 0 && b1 == cap1)
