module Change (findFewestCoins) where

import Data.Array (Array, listArray, (!))
import Data.List (minimumBy, sort)
import Data.Function (on)
import Data.Maybe (mapMaybe)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins_
  | target < 0   = Nothing
  | target == 0  = Just []
  | otherwise = 
      let validCoins = filter (>0) coins_
      in if null validCoins 
         then Nothing
         else if target > fromIntegral (maxBound :: Int) 
              then Nothing  -- Amount too large for Int conversion
              else 
                  let amt = fromInteger target :: Int
                      dp = listArray (0, amt) [ compute i | i <- [0..amt] ]
                      compute 0 = Just []
                      compute i = 
                          let candidates = mapMaybe (\c -> 
                                      let cInt = fromInteger c
                                      in if cInt > i 
                                         then Nothing
                                         else case dp ! (i - cInt) of
                                                Just prevCoins -> Just (c : prevCoins)
                                                Nothing -> Nothing
                                  ) validCoins
                          in if null candidates
                             then Nothing
                             else Just (minimumBy (compare `on` length) candidates)
                  in fmap sort (dp ! amt)
