module Yacht (yacht, Category(..)) where

import Data.List (sort, group)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht category dice = case category of
    Ones          -> sumOf 1
    Twos          -> sumOf 2
    Threes        -> sumOf 3
    Fours         -> sumOf 4
    Fives         -> sumOf 5
    Sixes         -> sumOf 6
    FullHouse     -> if isFullHouse then sum dice else 0
    FourOfAKind   -> case fourOfAKindValue of
                        Just v  -> 4 * v
                        Nothing -> 0
    LittleStraight -> if sort dice == [1,2,3,4,5] then 30 else 0
    BigStraight    -> if sort dice == [2,3,4,5,6] then 30 else 0
    Choice        -> sum dice
    Yacht         -> if allEqual then 50 else 0
  where
    sumOf n = n * length (filter (== n) dice)
    groups = group (sort dice)
    counts = map length groups
    values = map head groups
    isFullHouse = length groups == 2 && (sort counts == [2,3])
    fourOfAKindValue = case filter (\g -> length g >= 4) groups of
                        (g:_) -> Just (head g)
                        []    -> Nothing
    allEqual = length groups == 1
