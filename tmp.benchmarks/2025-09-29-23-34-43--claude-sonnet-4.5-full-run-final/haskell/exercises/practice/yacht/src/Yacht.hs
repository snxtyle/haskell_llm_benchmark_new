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
yacht Ones dice = sum [d | d <- dice, d == 1]
yacht Twos dice = sum [d | d <- dice, d == 2]
yacht Threes dice = sum [d | d <- dice, d == 3]
yacht Fours dice = sum [d | d <- dice, d == 4]
yacht Fives dice = sum [d | d <- dice, d == 5]
yacht Sixes dice = sum [d | d <- dice, d == 6]
yacht FullHouse dice = if isFullHouse dice then sum dice else 0
yacht FourOfAKind dice = scoreFourOfAKind dice
yacht LittleStraight dice = if sort dice == [1,2,3,4,5] then 30 else 0
yacht BigStraight dice = if sort dice == [2,3,4,5,6] then 30 else 0
yacht Choice dice = sum dice
yacht Yacht dice = if isYacht dice then 50 else 0

-- Helper function to check if all dice show the same face
isYacht :: [Int] -> Bool
isYacht dice = all (== head dice) dice

-- Helper function to check for Full House (three of one number and two of another)
isFullHouse :: [Int] -> Bool
isFullHouse dice = 
    let counts = map length $ group $ sort dice
    in sort counts == [2, 3]

-- Helper function to score Four of a Kind
scoreFourOfAKind :: [Int] -> Int
scoreFourOfAKind dice =
    let grouped = group $ sort dice
        fourOfAKindGroup = filter (\g -> length g >= 4) grouped
    in case fourOfAKindGroup of
        (g:_) -> 4 * head g
        []    -> 0
