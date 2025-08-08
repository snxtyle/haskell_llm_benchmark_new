module Yacht (yacht, Category(..)) where

import Data.List (group, sort)

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
yacht Ones dice = sum [d | d <- dice, d == 1]
yacht Twos dice = sum [d | d <- dice, d == 2]
yacht Threes dice = sum [d | d <- dice, d == 3]
yacht Fours dice = sum [d | d <- dice, d == 4]
yacht Fives dice = sum [d | d <- dice, d == 5]
yacht Sixes dice = sum [d | d <- dice, d == 6]
yacht FullHouse dice = 
    let groups = sort $ map length $ group $ sort dice
    in if groups == [2, 3] then sum dice else 0
yacht FourOfAKind dice =
    let groups = group $ sort dice
        fourOrMore = filter (\g -> length g >= 4) groups
    in case fourOrMore of
        (g:_) -> sum $ take 4 g
        [] -> 0
yacht LittleStraight dice =
    if sort dice == [1, 2, 3, 4, 5] then 30 else 0
yacht BigStraight dice =
    if sort dice == [2, 3, 4, 5, 6] then 30 else 0
yacht Choice dice = sum dice
yacht Yacht dice =
    let groups = group $ sort dice
    in if length groups == 1 && length (head groups) == 5 then 50 else 0
