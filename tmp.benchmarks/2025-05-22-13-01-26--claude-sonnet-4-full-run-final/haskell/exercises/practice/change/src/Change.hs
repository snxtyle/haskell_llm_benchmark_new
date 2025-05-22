module Change (findFewestCoins) where

import qualified Data.Map as Map
import Data.List (minimumBy)
import Data.Ord (comparing)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins = Nothing
  | all (> target) coins = Nothing
  | otherwise = fst $ dpHelper target coins Map.empty
  where
    dpHelper :: Integer -> [Integer] -> Map.Map Integer (Maybe [Integer]) -> (Maybe [Integer], Map.Map Integer (Maybe [Integer]))
    dpHelper 0 _ memo = (Just [], memo)
    dpHelper amt _ memo | amt < 0 = (Nothing, memo)
    dpHelper amt coinList memo =
      case Map.lookup amt memo of
        Just result -> (result, memo)
        Nothing -> 
          let validCoins = filter (<= amt) coinList
              (results, finalMemo) = foldr processCoins ([], memo) validCoins
              validResults = [r | Just r <- results]
              result = if null validResults 
                      then Nothing 
                      else Just $ minimumBy (comparing length) validResults
              newMemo = Map.insert amt result finalMemo
          in (result, newMemo)
      where
        processCoins coin (acc, currentMemo) =
          let (subResult, updatedMemo) = dpHelper (amt - coin) coinList currentMemo
          in case subResult of
               Just rest -> (Just (coin : rest) : acc, updatedMemo)
               Nothing -> (Nothing : acc, updatedMemo)
