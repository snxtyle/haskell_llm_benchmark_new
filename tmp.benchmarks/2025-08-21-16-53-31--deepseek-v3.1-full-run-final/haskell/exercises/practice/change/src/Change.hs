module Change (findFewestCoins) where

import Data.Maybe (listToMaybe)
import Data.List (sortBy)
import qualified Data.Map as Map

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null validCoins = Nothing
  | otherwise = fst $ dp target Map.empty
  where
    validCoins = filter (> 0) coins
    sortedCoins = sortBy (flip compare) validCoins
    
    dp :: Integer -> Map.Map Integer (Maybe [Integer]) -> (Maybe [Integer], Map.Map Integer (Maybe [Integer]))
    dp 0 memo = (Just [], memo)
    dp n memo
      | n < 0 = (Nothing, memo)
      | otherwise = case Map.lookup n memo of
          Just result -> (result, memo)
          Nothing -> 
            let (result, finalMemo) = foldl (tryCoin n) (Nothing, memo) sortedCoins
            in (result, Map.insert n result finalMemo)
    
    tryCoin :: Integer -> (Maybe [Integer], Map.Map Integer (Maybe [Integer])) -> Integer -> (Maybe [Integer], Map.Map Integer (Maybe [Integer]))
    tryCoin n (currentBest, memo) coin
      | coin > n = (currentBest, memo)
      | otherwise = case dp (n - coin) memo of
          (Just rest, memo') -> 
            let newSolution = Just (coin : rest)
            in case currentBest of
                 Just best -> 
                   if length newSolution < length best
                   then (newSolution, memo')
                   else (currentBest, memo')
                 Nothing -> (newSolution, memo')
          (Nothing, memo') -> (currentBest, memo')
