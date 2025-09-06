module TwoBucket (measure) where

import Data.List (nub)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target = 
    case bfs [(1, cap1, 0)] [] of
        Just (actions, a, b) -> 
            if a == target
                then Just (actions, (a, b))
                else Just (actions, (b, a))
        Nothing -> Nothing
    where
        bfs [] _ = Nothing
        bfs ((actions, a, b):queue) visited
            | a == target || b == target = Just (actions, a, b)
            | (a, b) `elem` visited = bfs queue visited
            | otherwise = bfs (queue ++ newStates) ((a, b):visited)
            where
                newStates = map (\(a', b') -> (actions + 1, a', b')) $
                    filter (\(a', b') -> not (illegalState a' b')) $
                    nub $ generateNewStates (a, b) cap1 cap2
                
        illegalState a b = a == 0 && b == cap2
        
        generateNewStates (a, b) c1 c2 =
            [ -- Fill bucket1
              (c1, b)
            -- Fill bucket2
            , (a, c2)
            -- Empty bucket1
            , (0, b)
            -- Empty bucket2
            , (a, 0)
            -- Pour from bucket1 to bucket2
            , let amount = min a (c2 - b) in (a - amount, b + amount)
            -- Pour from bucket2 to bucket1
            , let amount = min b (c1 - a) in (a + amount, b - amount)
            ]
