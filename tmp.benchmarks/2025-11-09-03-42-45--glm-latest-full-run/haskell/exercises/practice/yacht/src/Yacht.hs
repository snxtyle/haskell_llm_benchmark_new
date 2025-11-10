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
yacht category dice =
    let sortedDice = sort dice
        counts = map length $ group sortedDice
        isYacht = counts == [5]
        isFullHouse = counts `elem` [[2, 3], [3, 2]]
        isLittleStraight = sortedDice == [1, 2, 3, 4, 5]
        isBigStraight = sortedDice == [2, 3, 4, 5, 6]
    in case category of
        Ones            -> sum $ filter (== 1) dice
        Twos            -> sum $ filter (== 2) dice
        Threes          -> sum $ filter (== 3) dice
        Fours           -> sum $ filter (== 4) dice
        Fives           -> sum $ filter (== 5) dice
        Sixes           -> sum $ filter (== 6) dice
        FullHouse       -> if isFullHouse then sum dice else 0
        FourOfAKind     ->
            let firstFour = take 4 sortedDice
                lastFour  = take 4 (drop 1 sortedDice)
                isFourOfAKind ds = length (group ds) == 1
            in if isFourOfAKind firstFour then sum firstFour
               else if isFourOfAKind lastFour then sum lastFour
               else 0
        LittleStraight  -> if isLittleStraight then 30 else 0
        BigStraight     -> if isBigStraight then 30 else 0
        Choice          -> sum dice
        Yacht           -> if isYacht then 50 else 0
