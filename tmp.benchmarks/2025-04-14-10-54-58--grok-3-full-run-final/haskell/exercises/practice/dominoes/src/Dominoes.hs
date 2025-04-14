module Dominoes (chain) where

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = findChain dominoes []

findChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
findChain remaining current
  | null remaining && isValidChain current = Just current
  | null remaining = Nothing
  | null current = tryAllFirst remaining current
  | otherwise = tryNext remaining current

isValidChain :: [(Int, Int)] -> Bool
isValidChain [] = True
isValidChain [(a, b)] = a == b
isValidChain chain = 
  let (firstA, _) = head chain
      (_, lastB) = last chain
  in firstA == lastB && all adjacentMatch (zip chain (tail chain))
  where
    adjacentMatch ((_, b), (a, _)) = b == a

tryAllFirst :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
tryAllFirst dominoes current = go dominoes
  where
    go [] = Nothing
    go (d:ds) = 
      case findChain ds (d:current) of
        Just result -> Just result
        Nothing -> 
          case findChain ds (flipDomino d:current) of
            Just result -> Just result
            Nothing -> go ds

tryNext :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
tryNext dominoes current = go dominoes
  where
    go [] = Nothing
    go (d:ds) = 
      if canConnect d (head current)
        then case findChain ds (d:current) of
               Just result -> Just result
               Nothing -> go ds
        else if canConnect (flipDomino d) (head current)
          then case findChain ds (flipDomino d:current) of
                 Just result -> Just result
                 Nothing -> go ds
          else go ds

canConnect :: (Int, Int) -> (Int, Int) -> Bool
canConnect (_, b) (a, _) = b == a

flipDomino :: (Int, Int) -> (Int, Int)
flipDomino (a, b) = (b, a)
