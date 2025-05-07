module Dominoes (chain) where

import Data.List (delete)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = tryStart dominoes
  where
    tryStart [] = Nothing
    tryStart (d:ds) =
      case tryChain [d] (delete d dominoes) of
        Just c -> Just c
        Nothing -> tryStart ds

    tryChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
    tryChain chainSoFar [] =
      if fst (head chainSoFar) == snd (last chainSoFar)
        then Just chainSoFar
        else Nothing
    tryChain chainSoFar remaining =
      let lastRight = snd (last chainSoFar)
          tryNext [] = Nothing
          tryNext (x:xs) =
            let (a,b) = x
                tryAppend stone =
                  tryChain (chainSoFar ++ [stone]) (delete x remaining)
                tryBoth =
                  case tryAppend (a,b) of
                    Just c -> Just c
                    Nothing -> tryAppend (b,a)
                -- Prevent using the same domino in both directions consecutively
                isValid stone =
                  let (_, lastR) = last chainSoFar
                      (left, right) = stone
                  in lastR == left && not (stone `elem` chainSoFar)
            in if isValid (a,b)
                 then case tryAppend (a,b) of
                        Just c -> Just c
                        Nothing -> if isValid (b,a)
                                     then tryAppend (b,a)
                                     else tryNext xs
                 else if isValid (b,a)
                        then tryAppend (b,a)
                        else tryNext xs
      in tryNext remaining
