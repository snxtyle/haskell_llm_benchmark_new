module Change (findFewestCoins) where

import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null validCoins = Nothing
  | target < minimum validCoins = Nothing
  | otherwise = fmap snd $ Map.lookup target memo
  where
    validCoins = filter (> 0) coins
    memo = Map.fromList $ (0, Just []) : [(amt, bestChange amt) | amt <- [1..target]]
    bestChange amt = 
      let candidates = [ fmap (coin :) (Map.lookup (amt - coin) memo) | coin <- validCoins, coin <= amt ]
          validCandidates = catMaybes candidates
      in if null validCandidates then Nothing else Just $ minimumBy (comparing length) validCandidates
