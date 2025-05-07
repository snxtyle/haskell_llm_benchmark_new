module Dominoes (chain) where

-- Helper to remove an element at a specific index from a list.
-- Example: removeAt 1 [10,20,30] == [10,30]
-- Indices are 0-based.
removeAt :: Int -> [a] -> [a]
removeAt idx xs = take idx xs ++ drop (idx + 1) xs

-- Helper to process a list of items with a function that might return a value (Just) or not (Nothing).
-- Returns the first Just value encountered, or Nothing if all applications result in Nothing.
-- This allows for short-circuiting: computation stops once a solution is found.
firstSuccess :: (a -> Maybe b) -> [a] -> Maybe b
firstSuccess _ [] = Nothing
firstSuccess f (x:xs) =
    case f x of
        Just result -> Just result
        Nothing     -> firstSuccess f xs

-- | Computes a way to order a given set of dominoes to form a valid chain.
-- A valid chain means:
-- 1. Adjacent dominoes match (e.g., (a,b) is followed by (b,c)).
-- 2. All input dominoes are used exactly once.
-- 3. The first and last pips in the entire chain match.
-- Dominoes can be flipped (e.g., (a,b) can be used as (b,a)).
chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just [] -- An empty list of dominoes forms an empty, valid chain.
chain dominoesList =
    -- Try to start a chain with each domino in the input list.
    -- The 'firstSuccess' helper will return the first valid chain found.
    firstSuccess tryStartingWith dominoIdxs
  where
    dominoIdxs = [0..length dominoesList - 1]

    -- Check if a completed chain (using all dominoes) is valid.
    -- For a non-empty chain, this means the first pip of the first domino
    -- matches the second pip of the last domino.
    isChainValid :: [(Int, Int)] -> Bool
    -- This function is called with a non-empty chain, so head/last are safe.
    isChainValid ch = fst (head ch) == snd (last ch)

    -- Tries to form a chain starting with the domino at 'startIndex'.
    -- It tries both orientations of the starting domino.
    tryStartingWith :: Int -> Maybe [(Int, Int)]
    tryStartingWith startIndex =
        let (d1, d2)    = dominoesList !! startIndex
            remaining   = removeAt startIndex dominoesList
            
            -- Helper to attempt building a chain and then check its validity.
            checkedAttempt :: Maybe [(Int, Int)] -> Maybe [(Int, Int)]
            checkedAttempt Nothing = Nothing
            checkedAttempt (Just chainResult) =
                if isChainValid chainResult then Just chainResult else Nothing

            -- Attempt 1: Start with (d1, d2)
            option1 = checkedAttempt (attemptBuildChain [(d1, d2)] d2 remaining)
        in
        case option1 of
            Just _ -> option1 -- Found a valid chain starting with (d1, d2)
            Nothing -> -- If option1 failed, try starting with (d2, d1), if not a double
                if d1 == d2
                then Nothing -- Domino is a double, flipping is redundant
                else checkedAttempt (attemptBuildChain [(d2, d1)] d1 remaining)


    -- Recursively attempts to extend 'currentPartialChain' using 'availableDominoes'.
    -- 'currentPartialChain': The list of dominoes forming the chain so far.
    -- 'lastPipToMatch': The pip value that the next domino must start with.
    -- 'availableDominoes': The list of dominoes not yet used in the chain.
    attemptBuildChain :: [(Int, Int)] -> Int -> [(Int, Int)] -> Maybe [(Int, Int)]
    attemptBuildChain currentPartialChain _ [] =
        Just currentPartialChain -- Base case: All dominoes used, return the chain.
                                 -- Overall validity (first/last pips) is checked by the caller ('tryStartingWith').

    attemptBuildChain currentPartialChain lastPipToMatch availableDominoes =
        -- Try to extend the chain with each of the available dominoes.
        firstSuccess tryNextWithCandidate candidateInfos
      where
        -- Create pairs of (index, domino) for available dominoes to handle removal by index.
        candidateInfos = zip [0..] availableDominoes

        -- Tries to use 'candidateDomino' (at 'candidateIdx') to extend the chain.
        tryNextWithCandidate :: (Int, (Int, Int)) -> Maybe [(Int, Int)]
        tryNextWithCandidate (candidateIdx, (c1, c2)) =
            let remainingAfterCandidate = removeAt candidateIdx availableDominoes
            in
            -- Attempt 1: Use candidate as (c1, c2)
            if c1 == lastPipToMatch then
                attemptBuildChain (currentPartialChain ++ [(c1, c2)]) c2 remainingAfterCandidate
            -- Attempt 2: Use candidate as (c2, c1), if it's not a double and matches
            else if c1 /= c2 && c2 == lastPipToMatch then
                attemptBuildChain (currentPartialChain ++ [(c2, c1)]) c1 remainingAfterCandidate
            -- Otherwise, this candidate cannot extend the chain in either orientation
            else
                Nothing
