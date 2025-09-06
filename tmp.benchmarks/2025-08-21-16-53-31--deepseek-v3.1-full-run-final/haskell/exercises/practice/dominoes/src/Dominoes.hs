module Dominoes (chain) where

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = findChain dominoes [] Nothing

findChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe Int -> Maybe [(Int, Int)]
findChain [] result Nothing = Just result
findChain [] result (Just first)
    | not (null result) && snd (last result) == first = Just result
    | otherwise = Nothing
findChain dominoes result target = tryDominoes dominoes
  where
    tryDominoes [] = Nothing
    tryDominoes (d:ds) =
        case tryOrientation d of
            Just solution -> Just solution
            Nothing -> tryDominoes ds
    
    tryOrientation (a, b) =
        case target of
            Nothing -> 
                -- First domino in chain
                findChain ds [(a, b)] (Just a) `orElse`
                findChain ds [(b, a)] (Just b)
            Just t ->
                -- Need to match the target
                if a == t then findChain ds (result ++ [(a, b)]) target `orElse` next
                else if b == t then findChain ds (result ++ [(b, a)]) target `orElse` next
                else next
      where
        next = tryDominoes ds
    
    orElse Nothing y = y
    orElse x _ = x
