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
yacht category dice =
  case category of
    Ones -> scoreNumber 1
    Twos -> scoreNumber 2
    Threes -> scoreNumber 3
    Fours -> scoreNumber 4
    Fives -> scoreNumber 5
    Sixes -> scoreNumber 6
    FullHouse -> if isFullHouse then sum dice else 0
    FourOfAKind -> if maxGroup >= 4 then value * 4 else 0
    LittleStraight -> if sorted == [1,2,3,4,5] then 30 else 0
    BigStraight -> if sorted == [2,3,4,5,6] then 30 else 0
    Choice -> sum dice
    Yacht -> if allEqual then 50 else 0
  where
    sorted = sort dice
    groups = group sorted
    lengths = map length groups
    sortedLengths = sort lengths
    isFullHouse = sortedLengths == [2,3]
    maxGroup = maximum lengths
    value = head $ head $ filter (\g -> length g >= 4) groups
    allEqual = all (== head dice) dice
    scoreNumber n = n * length (filter (== n) dice)
