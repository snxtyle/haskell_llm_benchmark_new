module Dominoes (chain) where

import Data.List (filter)

-- Helper to find a chain recursively
-- currentEnd: The number at the open end of the current chain.
-- firstDomino: The very first domino placed in the chain (to check circularity).
-- availableDominoes: List of (original_index, domino) pairs not yet used.
-- currentChain: The chain built so far, in reverse order for efficient consing.
solve :: Int -> (Int, Int) -> [(Int, (Int, Int))] -> [(Int, Int)] -> Maybe [(Int, Int)]
solve currentEnd firstDomino availableDominoes currentChain
    -- Base case: All dominoes have been used.
    | null availableDominoes =
        let firstNumOfChain = fst firstDomino
        in if currentEnd == firstNumOfChain
           then Just (reverse currentChain)
           else Nothing
    -- Recursive step: Try to add a domino.
    | otherwise =
        -- tryNextDomino iterates through the available dominoes and tries to extend the chain.
        -- It returns the first successful chain found, or Nothing if no path from this state works.
        let
            tryNextDomino :: [(Int, (Int, Int))] -> Maybe [(Int, Int)]
            tryNextDomino [] = Nothing -- No more available dominoes to try from this branch
            tryNextDomino ((idx, (x, y)) : restOfAvailable) =
                let
                    -- Create a new list of available dominoes, excluding the one currently being tried (by its unique index)
                    newAvailable = filter (\(i, _) -> i /= idx) availableDominoes
                in
                -- Option 1: Try using (x, y) if x matches currentEnd
                if x == currentEnd
                then
                    let res = solve y firstDomino newAvailable ((x, y) : currentChain)
                    in case res of
                        Just _ -> res -- Found a solution, propagate it up
                        Nothing -> tryNextDomino restOfAvailable -- No solution with (x,y) in this orientation, try next available domino
                -- Option 2: Try using (y, x) if y matches currentEnd
                else if y == currentEnd
                then
                    let res = solve x firstDomino newAvailable ((y, x) : currentChain)
                    in case res of
                        Just _ -> res -- Found a solution, propagate it up
                        Nothing -> tryNextDomino restOfAvailable -- No solution with (y,x) in this orientation, try next available domino
                -- If neither (x,y) nor (y,x) matches currentEnd, try the next available domino
                else tryNextDomino restOfAvailable
        in
        tryNextDomino availableDominoes

-- Main function to find a domino chain.
chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just [] -- An empty list of dominoes forms a valid empty chain.
chain dominoes =
    let
        -- Assign unique indices to dominoes to handle duplicates correctly.
        -- This allows us to "remove" a specific instance of a domino.
        dominoesWithIndices = zip [0..] dominoes
    in
    -- tryStartingDomino iterates through each domino in the input list and tries to use it as the first domino in the chain.
    -- It returns the first successful chain found, or Nothing if no chain can be formed.
    let
        tryStartingDomino :: [(Int, (Int, Int))] -> Maybe [(Int, Int)]
        tryStartingDomino [] = Nothing -- No more dominoes to try as a starting point
        tryStartingDomino ((idx, (a, b)) : restDominoesWithIndices) =
            let
                -- Create the list of remaining dominoes for the recursive 'solve' function,
                -- excluding the one chosen as the starting point.
                remainingForSolve = filter (\(i, _) -> i /= idx) dominoesWithIndices
            in
            -- Option 1: Start the chain with (a, b)
            let res1 = solve b (a, b) remainingForSolve [(a, b)]
            in case res1 of
                Just _ -> res1 -- Found a solution, propagate it up
                Nothing ->
                    -- Option 2: Start the chain with (b, a) (flipped)
                    let res2 = solve a (b, a) remainingForSolve [(b, a)]
                    in case res2 of
                        Just _ -> res2 -- Found a solution, propagate it up
                        Nothing -> tryStartingDomino restDominoesWithIndices -- No solution with this starting domino, try the next one
    in
    tryStartingDomino dominoesWithIndices
