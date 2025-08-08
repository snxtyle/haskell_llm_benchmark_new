module Change (findFewestCoins) where

import Data.Array (Array, listArray, (!))
import qualified Data.List as L
import Data.Maybe (catMaybes)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins' = Nothing
  | otherwise =
      let t = toInt target
          dp :: Array Int (Maybe (Int, [Integer]))
          dp = listArray (0, t) [entry a | a <- [0..t]]

          entry :: Int -> Maybe (Int, [Integer])
          entry 0 = Just (0, [])
          entry a =
            let candidates =
                  [ fmap (\(cnt, xs) -> (cnt + 1, c : xs)) (dp ! (a - ci))
                  | c <- coins'
                  , let ci = toInt c
                  , ci <= a
                  ]
            in chooseBest candidates
      in case dp ! t of
           Nothing -> Nothing
           Just (_, xs) -> Just (L.sort xs)
  where
    -- Keep only positive coins, sort for determinism, and drop duplicates
    coins' :: [Integer]
    coins' = L.nub . L.sort $ filter (> 0) coins

    toInt :: Integer -> Int
    toInt = fromInteger

    chooseBest :: [Maybe (Int, [Integer])] -> Maybe (Int, [Integer])
    chooseBest ms =
      case catMaybes ms of
        [] -> Nothing
        xs -> Just (L.minimumBy cmp xs)

    -- Prefer fewer coins; if tied, prefer lexicographically smaller
    -- (after sorting) for determinism.
    cmp :: (Int, [Integer]) -> (Int, [Integer]) -> Ordering
    cmp (c1, xs1) (c2, xs2) =
      let ord = compare c1 c2
      in if ord == EQ
           then compare (L.sort xs1) (L.sort xs2)
           else ord
