module Dominoes (chain) where

import Data.Maybe (listToMaybe, catMaybes)

-- | Compute a domino chain that uses every given stone exactly once,
--   such that adjacent stones share a number and the resulting chain
--   is closed (the first number equals the last number).
--
--   If such a chain exists the function returns @Just chain@, otherwise
--   @Nothing@.
--
--   The orientation of every individual stone in the resulting list is
--   significant – the second element of each tuple must match the first
--   element of the next tuple.
chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes =
  listToMaybe $ catMaybes
    [ tryChain startOrient i
    | (i, stone) <- zip [0 ..] dominoes
    , startOrient <- orientations stone
    ]
  where
    -- Attempt to build a full chain starting with the given oriented stone.
    tryChain :: (Int, Int) -> Int -> Maybe [(Int, Int)]
    tryChain (firstLeft, firstRight) idx =
      extend [ (firstLeft, firstRight) ] firstLeft firstRight (removeAt idx dominoes)

    -- extend currentChain firstVal lastVal remainingStones
    extend :: [(Int, Int)] -> Int -> Int -> [(Int, Int)] -> Maybe [(Int, Int)]
    extend currentChain firstVal lastVal [] =
      if lastVal == firstVal
        then Just (reverse currentChain)
        else Nothing
    extend currentChain firstVal lastVal remaining =
      listToMaybe $ catMaybes
        [ extend (nextOrient : currentChain) firstVal nextRight (removeAt j remaining)
        | (j, stone) <- zip [0 ..] remaining
        , nextOrient@(nextLeft, nextRight) <- orientations stone
        , nextLeft == lastVal
        ]

-- | All possible orientations of a domino.  If both halves are the same
--   there is only one orientation.
orientations :: (Int, Int) -> [(Int, Int)]
orientations (a, b)
  | a == b    = [(a, b)]
  | otherwise = [(a, b), (b, a)]

-- | Remove the element at the given index from a list.
removeAt :: Int -> [a] -> [a]
removeAt idx xs = let (front, rest) = splitAt idx xs in front ++ drop 1 rest
