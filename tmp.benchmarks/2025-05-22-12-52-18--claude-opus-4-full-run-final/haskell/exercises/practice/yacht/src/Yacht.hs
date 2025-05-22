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
yacht category dice = case category of
    Ones   -> scoreNumber 1 dice
    Twos   -> scoreNumber 2 dice
    Threes -> scoreNumber 3 dice
    Fours  -> scoreNumber 4 dice
    Fives  -> scoreNumber 5 dice
    Sixes  -> scoreNumber 6 dice
    FullHouse -> scoreFullHouse dice
    FourOfAKind -> scoreFourOfAKind dice
    LittleStraight -> scoreLittleStraight dice
    BigStraight -> scoreBigStraight dice
    Choice -> sum dice
    Yacht -> scoreYacht dice

-- Score for number categories (Ones through Sixes)
scoreNumber :: Int -> [Int] -> Int
scoreNumber n dice = n * length (filter (== n) dice)

-- Score for Full House (three of one number and two of another)
scoreFullHouse :: [Int] -> Int
scoreFullHouse dice =
    let groups = group $ sort dice
        lengths = map length groups
    in if sort lengths == [2, 3]
       then sum dice
       else 0

-- Score for Four of a Kind (at least four dice showing the same face)
scoreFourOfAKind :: [Int] -> Int
scoreFourOfAKind dice =
    let groups = group $ sort dice
        fourOrMore = filter (\g -> length g >= 4) groups
    in case fourOrMore of
        (g:_) -> 4 * head g
        []    -> 0

-- Score for Little Straight (1-2-3-4-5)
scoreLittleStraight :: [Int] -> Int
scoreLittleStraight dice =
    if sort dice == [1, 2, 3, 4, 5]
    then 30
    else 0

-- Score for Big Straight (2-3-4-5-6)
scoreBigStraight :: [Int] -> Int
scoreBigStraight dice =
    if sort dice == [2, 3, 4, 5, 6]
    then 30
    else 0

-- Score for Yacht (all five dice showing the same face)
scoreYacht :: [Int] -> Int
scoreYacht dice =
    if all (== head dice) dice
    then 50
    else 0
