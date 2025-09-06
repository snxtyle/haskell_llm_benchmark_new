module Change (findFewestCoins) where

import Data.Array
import Data.List (sort, nub, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | otherwise =
      let validCoins = sort $ nub $ filter (> 0) coins
          bounds = (0, target)
          dp = array bounds [(i, compute i validCoins dp) | i <- range bounds]
      in dp ! target
  where
    compute 0 _ _ = Just []
    compute i cs dp
      | i < 0 = Nothing
      | otherwise =
          let options = [fmap (c :) (dp ! (i - c)) | c <- cs, i >= c]
              validOptions = catMaybes options
          in if null validOptions
             then Nothing
             else Just $ minimumBy (comparing length) validOptions
