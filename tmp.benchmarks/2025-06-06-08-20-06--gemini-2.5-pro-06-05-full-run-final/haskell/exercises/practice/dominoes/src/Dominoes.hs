module Dominoes (chain) where

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes =
    case solutions of
        (s:_) -> Just s
        []    -> Nothing
  where
    -- Generate all possible solutions by trying every domino as a starting point.
    solutions = concatMap tryStart (select dominoes)

    -- Tries a domino as a starting point, in both orientations.
    -- Returns a list of all valid chains found starting with this domino.
    tryStart :: ((Int, Int), [(Int, Int)]) -> [[(Int, Int)]]
    tryStart (startDomino, rest) =
        let (a, b) = startDomino
            -- Try starting with (a,b)
            sol1 = solve [(a,b)] rest
            -- Try starting with (b,a), if not a symmetric domino like (2,2)
            sol2 = if a /= b then solve [(b,a)] rest else []
        in sol1 ++ sol2

    -- Recursively search for a valid chain.
    -- `partialChain`: the chain built so far.
    -- `remaining`: the dominoes left to use.
    -- Returns a list of all valid chains that can be formed.
    solve :: [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
    solve partialChain [] =
        let (startA, _) = head partialChain
            (_, endB)   = last partialChain
        in if startA == endB then [partialChain] else []
    solve partialChain remaining =
        let (_, currentEnd) = last partialChain
            -- Find dominoes that can be placed next and try to solve from there.
        in concatMap (\(nextDomino, rest) -> solve (partialChain ++ [nextDomino]) rest) (findNextMoves currentEnd remaining)

    -- Finds all possible next moves from the current state.
    -- A move consists of the next domino (oriented correctly) and the new list of remaining dominoes.
    findNextMoves :: Int -> [(Int, Int)] -> [((Int, Int), [(Int, Int)])]
    findNextMoves currentEnd remainingDominoes = concatMap (canPlace currentEnd) (select remainingDominoes)
      where
        canPlace val ((a,b), rest)
            | a == val  = [((a,b), rest)]
            | b == val  = [((b,a), rest)]
            | otherwise = []

    -- Given a list, returns a list of pairs, where each pair consists of an element
    -- of the list and the list of the other elements.
    select :: [a] -> [(a, [a])]
    select [] = []
    select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]
