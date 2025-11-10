module Dominoes (chain) where

import Data.List (delete)
import Data.Maybe (listToMaybe)

-- | Determine if a given set of dominoes can be arranged into a valid chain.
--
-- A chain is valid if:
-- 1. Every domino is used exactly once.
-- 2. Adjacent dominoes have matching numbers (e.g., (a,b) next to (c,d) requires b == c).
-- 3. The chain forms a loop, where the first number of the first domino
--    matches the second number of the last domino.
--
-- Dominoes can be flipped. For example, (1,2) can be used as (1,2) or (2,1).
--
-- If a valid chain exists, it is returned wrapped in 'Just'. Otherwise, 'Nothing' is returned.
chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = listToMaybe $ concatMap tryStart dominoes
  where
    -- Try to build a chain starting with a specific domino in both orientations.
    tryStart :: (Int, Int) -> [[(Int, Int)]]
    tryStart d@(a, b) =
        let remaining = delete d dominoes
        in search [(a, b)] remaining ++ search [(b, a)] remaining

-- | Recursive helper to find all valid chains from a given state.
search :: [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
search path remaining
    -- Base case: no more dominoes to place.
    | null remaining =
        -- Check if the path forms a valid loop.
        let (start, _) = head path
            (_, end) = last path
        in if start == end
           then [path] -- Found a valid chain.
           else []     -- Dead end.
    -- Recursive step: try to extend the path.
    | otherwise =
        let (_, end) = last path
            -- Find all dominoes that can be placed next.
            possibleNext = filter (\(a, b) -> a == end || b == end) remaining
        in concatMap (tryExtend end) possibleNext
  where
    -- Try to extend the path with a specific domino.
    tryExtend :: Int -> (Int, Int) -> [[(Int, Int)]]
    tryExtend currentEnd d@(a, b)
        | a == currentEnd =
            let newPath = path ++ [(a, b)]
                newRemaining = delete d remaining
            in search newPath newRemaining
        | b == currentEnd =
            let newPath = path ++ [(b, a)]
                newRemaining = delete d remaining
            in search newPath newRemaining
        | otherwise = [] -- Should not be reached due to filter.
