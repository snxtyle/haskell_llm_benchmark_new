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
    Ones   -> countValue 1 dice
    Twos   -> countValue 2 dice
    Threes -> countValue 3 dice
    Fours  -> countValue 4 dice
    Fives  -> countValue 5 dice
    Sixes  -> countValue 6 dice
    FullHouse -> if isFullHouse dice then sum dice else 0
    FourOfAKind -> fourOfAKindScore dice
    LittleStraight -> if isLittleStraight dice then 30 else 0
    BigStraight -> if isBigStraight dice then 30 else 0
    Choice -> sum dice
    Yacht -> if isYacht dice then 50 else 0

-- Count how many times a specific value appears and multiply by that value
countValue :: Int -> [Int] -> Int
countValue value dice = value * length (filter (== value) dice)

-- Check if dice form a full house (3 of one kind, 2 of another)
isFullHouse :: [Int] -> Bool
isFullHouse dice = 
    let counts = map length $ group $ sort dice
    in sort counts == [2, 3]

-- Score for four of a kind (sum of the four matching dice)
fourOfAKindScore :: [Int] -> Int
fourOfAKindScore dice =
    let groups = group $ sort dice
        fourOfAKindGroup = filter (\g -> length g >= 4) groups
    in case fourOfAKindGroup of
        (g:_) -> 4 * head g
        []    -> 0

-- Check if dice form little straight (1-2-3-4-5)
isLittleStraight :: [Int] -> Bool
isLittleStraight dice = sort dice == [1, 2, 3, 4, 5]

-- Check if dice form big straight (2-3-4-5-6)
isBigStraight :: [Int] -> Bool
isBigStraight dice = sort dice == [2, 3, 4, 5, 6]

-- Check if all dice show the same face
isYacht :: [Int] -> Bool
isYacht dice = 
    let groups = group $ sort dice
    in length groups == 1
