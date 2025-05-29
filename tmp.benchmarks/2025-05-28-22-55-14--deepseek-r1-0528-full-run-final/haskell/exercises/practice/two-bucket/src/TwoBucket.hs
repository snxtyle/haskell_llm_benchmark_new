module TwoBucket (measure) where

import qualified Data.Set as S

data StartingBucket = Bucket1 | Bucket2 deriving (Show, Eq, Ord)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure buckets target = measure' buckets target Bucket1

measure' :: (Int, Int) -> Int -> StartingBucket -> Maybe (Int, (Int, Int))
measure' (cap1, cap2) target start
  | target < 0 = Nothing
  | otherwise = 
      let (wanted, (capA, capB), startState) = case start of
                Bucket1 -> (capA, (cap1, cap2), (cap1, 0))
                Bucket2 -> (capB, (cap2, cap1), (cap2, 0))
          initialVisited = S.singleton startState
          initialQueue = [(startState, 1)]
          forbidden (a, b) = (a, b) == (0, capB)
          nextStates (a, b) = 
              [ (capA, b)        -- fill bucketA
              , (a, capB)        -- fill bucketB
              , (0, b)           -- empty bucketA
              , (a, 0)           -- empty bucketB
              , (a - amt1, b + amt1)  -- pour from bucketA to bucketB
              , (a + amt2, b - amt2)  -- pour from bucketB to bucketA
              ]
                where
                  amt1 = min a (capB - b)
                  amt2 = min b (capA - a)
          result = bfs capA capB target forbidden nextStates initialVisited initialQueue
      in case result of
            Just (moves, (amountA, amountB), which) ->
                if which == Bucket2 && start == Bucket2
                then Just (moves, (Bucket2, amountB, amountA))
                else Just (moves, (which, amountA, amountB))
            _ -> Nothing
  where
    bfs :: Int -> Int -> Int -> ((Int,Int) -> Bool) -> ((Int,Int) -> [(Int,Int)]) 
            -> S.Set (Int, Int) -> [((Int, Int), Int)] 
            -> Maybe (Int, (Int, Int), StartingBucket)
    bfs capA capB target forbidden next visited [] = Nothing
    bfs capA capB target forbidden next visited (((a, b), moves):q)
      | a == target = Just (moves, (a, b), Bucket1)
      | b == target = Just (moves, (a, b), Bucket2)
      | otherwise = 
          let nexts = next (a, b)
              validStates = filter (\s -> not (forbidden s) && not (S.member s visited)) nexts
              updatedVisited = foldl (flip S.insert) visited validStates
              updatedQueue = q ++ map (\s -> (s, moves + 1)) validStates
          in bfs capA capB target forbidden next updatedVisited updatedQueue
