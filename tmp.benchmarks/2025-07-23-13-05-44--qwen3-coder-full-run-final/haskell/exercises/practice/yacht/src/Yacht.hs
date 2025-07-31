module Yacht (yacht, Category(..)) where

import Data.List (sort)

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
    Ones -> sum $ filter (==1) dice
    Twos -> sum $ filter (==2) dice
    Threes -> sum $ filter (==3) dice
    Fours -> sum $ filter (==4) dice
    Fives -> sum $ filter (==5) dice
    Sixes -> sum $ filter (==6) dice
    FullHouse -> 
        let counts = map (\n -> length $ filter (==n) dice) [1..6]
            hasThree = 3 `elem` counts
            hasTwo = 2 `elem` counts
        in if hasThree && hasTwo then sum dice else 0
    FourOfAKind -> 
        let counts = map (\n -> (n, length $ filter (==n) dice)) [1..6]
            fourMatch = filter (\(_, count) -> count >= 4) counts
        in case fourMatch of
            [] -> 0
            (value, _) : _ -> 4 * value
    LittleStraight -> 
        if sort dice == [1,2,3,4,5] then 30 else 0
    BigStraight -> 
        if sort dice == [2,3,4,5,6] then 30 else 0
    Choice -> sum dice
    Yacht -> 
        if all (== head dice) dice then 50 else 0
