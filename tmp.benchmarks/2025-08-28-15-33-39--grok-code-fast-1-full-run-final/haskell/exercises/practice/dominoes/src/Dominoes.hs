module Dominoes (chain) where

import Data.List (permutations)
import Data.Maybe (mapMaybe, listToMaybe, maybeToList)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes =
  let degrees = computeDegrees dominoes
      allEven = all (even . snd) degrees
  in if not allEven then Nothing
     else listToMaybe $ mapMaybe findValid (permutations dominoes)

computeDegrees :: [(Int, Int)] -> [(Int, Int)]
computeDegrees = foldr update [] . concatMap (\(a, b) -> [a, b])
  where update x [] = [(x, 1)]
        update x ((y, n):ys) | x == y = (y, n + 1) : ys
                             | otherwise = (y, n) : update x ys

findValid :: [(Int, Int)] -> Maybe [(Int, Int)]
findValid perm = do
  oriented <- orientChain perm
  let start = fst (head oriented)
      end = snd (last oriented)
  if start == end then Just oriented else Nothing

orientChain :: [(Int, Int)] -> Maybe [(Int, Int)]
orientChain [] = Just []
orientChain (d:ds) = listToMaybe [ (a, b) : rest | (a, b) <- orients d, rest <- maybeToList (orientFrom b ds) ]

orients :: (Int, Int) -> [(Int, Int)]
orients (a, b) = if a == b then [(a, b)] else [(a, b), (b, a)]

orientFrom :: Int -> [(Int, Int)] -> Maybe [(Int, Int)]
orientFrom _ [] = Just []
orientFrom prev (d:ds) = listToMaybe [ (a, b) : rest | (a, b) <- orients d, a == prev, rest <- maybeToList (orientFrom b ds) ]
